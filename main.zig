const std = @import("std");
const search = @import("search.zig");
const exact = @import("exact.zig");

const Allocator = std.mem.Allocator;

const SearchState = struct {
    allocator: Allocator,
    mutex: std.Thread.Mutex = .{},
    lifecycle_mutex: std.Thread.Mutex = .{},
    iterator: search.Iterator,
    running: bool = false,
    completed: bool = false,
    failed: bool = false,
    tested: exact.BigInt,
    latest: []u8,
    failure: []u8,
    stage: []u8,
    global_index: []u8,
    local_index: []u8,
    started_ns: i128 = 0,
    thread: ?std.Thread = null,

    fn init(allocator: Allocator) !SearchState {
        var iterator = try search.Iterator.init(allocator);
        errdefer iterator.deinit();
        var tested = try exact.BigInt.initSet(allocator, 0);
        errdefer tested.deinit();
        return .{
            .allocator = allocator,
            .iterator = iterator,
            .tested = tested,
            .latest = try allocator.dupe(u8, "null"),
            .failure = try allocator.dupe(u8, ""),
            .stage = try allocator.dupe(u8, "0"),
            .global_index = try allocator.dupe(u8, "0"),
            .local_index = try allocator.dupe(u8, "0"),
        };
    }

    fn deinit(self: *SearchState) void {
        self.stop();
        self.iterator.deinit();
        self.tested.deinit();
        self.allocator.free(self.latest);
        self.allocator.free(self.failure);
        self.allocator.free(self.stage);
        self.allocator.free(self.global_index);
        self.allocator.free(self.local_index);
    }

    fn stop(self: *SearchState) void {
        self.lifecycle_mutex.lock();
        defer self.lifecycle_mutex.unlock();
        self.stopWorker();
    }

    fn stopWorker(self: *SearchState) void {
        self.mutex.lock();
        self.running = false;
        const thread = self.thread;
        self.thread = null;
        self.mutex.unlock();
        if (thread) |running_thread| running_thread.join();
    }

    fn isRunning(self: *SearchState) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.running;
    }

    fn start(self: *SearchState) !void {
        self.lifecycle_mutex.lock();
        defer self.lifecycle_mutex.unlock();
        self.stopWorker();
        var iterator = try search.Iterator.init(self.allocator);
        self.iterator.deinit();
        self.iterator = iterator;
        self.mutex.lock();
        self.completed = false;
        self.failed = false;
        self.running = true;
        self.started_ns = std.time.nanoTimestamp();
        self.tested.set(0) catch {
            self.running = false;
            self.mutex.unlock();
            return error.OutOfMemory;
        };
        self.replace(&self.latest, "null") catch {
            self.running = false;
            self.mutex.unlock();
            return error.OutOfMemory;
        };
        self.replace(&self.failure, "") catch {
            self.running = false;
            self.mutex.unlock();
            return error.OutOfMemory;
        };
        self.replace(&self.stage, "0") catch {
            self.running = false;
            self.mutex.unlock();
            return error.OutOfMemory;
        };
        self.replace(&self.global_index, "0") catch {
            self.running = false;
            self.mutex.unlock();
            return error.OutOfMemory;
        };
        self.replace(&self.local_index, "0") catch {
            self.running = false;
            self.mutex.unlock();
            return error.OutOfMemory;
        };
        self.mutex.unlock();
        const thread = std.Thread.spawn(.{}, runSearch, .{self}) catch {
            self.mutex.lock();
            self.running = false;
            self.mutex.unlock();
            return error.ThreadSpawnFailed;
        };
        self.mutex.lock();
        self.thread = thread;
        self.mutex.unlock();
    }

    fn recordWork(self: *SearchState, work: search.Work) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.replace(&self.stage, work.stage);
        try self.replace(&self.global_index, work.global_index);
        try self.replace(&self.local_index, work.local_index);
    }

    fn recordChecked(self: *SearchState) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try exact.BigInt.addScalar(&self.tested, &self.tested, 1);
    }

    fn publish(self: *SearchState, result: []const u8) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.replace(&self.latest, result);
        self.completed = true;
        self.running = false;
    }

    fn fail(self: *SearchState, message: []const u8) void {
        self.mutex.lock();
        self.replace(&self.failure, message) catch {};
        self.failed = true;
        self.running = false;
        self.mutex.unlock();
    }

    fn replace(self: *SearchState, destination: *[]u8, source: []const u8) !void {
        const copy = try self.allocator.dupe(u8, source);
        self.allocator.free(destination.*);
        destination.* = copy;
    }

    fn snapshot(self: *SearchState, allocator: Allocator) ![]u8 {
        self.mutex.lock();
        defer self.mutex.unlock();
        var output = std.ArrayList(u8).init(allocator);
        errdefer output.deinit();
        const tested = try self.tested.toString(allocator, 10, .lower);
        defer allocator.free(tested);
        const elapsed: u64 = if (self.started_ns == 0)
            0
        else
            @intCast(@divTrunc(std.time.nanoTimestamp() - self.started_ns, std.time.ns_per_s));
        try output.writer().print(
            "{{\"running\":{},\"completed\":{},\"failed\":{},\"stage\":\"{s}\",\"globalIndex\":\"{s}\",\"localIndex\":\"{s}\",\"tested\":\"{s}\",\"elapsedSeconds\":{},\"latest\":{s},\"failure\":",
            .{ self.running, self.completed, self.failed, self.stage, self.global_index, self.local_index, tested, elapsed, self.latest },
        );
        try appendJsonString(&output, self.failure);
        try output.append('}');
        return output.toOwnedSlice();
    }
};

fn runSearch(state: *SearchState) void {
    while (state.isRunning()) {
        var arena = std.heap.ArenaAllocator.init(state.allocator);
        defer arena.deinit();
        const allocator = arena.allocator();
        const work = state.iterator.next(allocator) catch |err| {
            state.fail(@errorName(err));
            return;
        };
        state.recordWork(work) catch |err| {
            state.fail(@errorName(err));
            return;
        };
        const candidate = searchCandidate(allocator, work) catch |err| {
            state.fail(@errorName(err));
            return;
        };
        const accepted = candidate.accepted() catch |err| {
            state.fail(@errorName(err));
            return;
        };
        state.recordChecked() catch |err| {
            state.fail(@errorName(err));
            return;
        };
        if (!accepted) continue;
        const result = candidate.json(
            allocator,
            work.stage,
            work.global_index,
            work.local_index,
            work.degree,
            work.codes,
        ) catch |err| {
            state.fail(@errorName(err));
            return;
        };
        state.publish(result) catch |err| {
            state.fail(@errorName(err));
            return;
        };
        return;
    }
}

fn searchCandidate(allocator: Allocator, work: search.Work) !exact.Candidate {
    return exact.Candidate.build(allocator, work.degree, work.modulus_degree, work.codes);
}

fn appendJsonString(output: *std.ArrayList(u8), value: []const u8) !void {
    try output.append('"');
    for (value) |character| {
        switch (character) {
            '"' => try output.appendSlice("\\\""),
            '\\' => try output.appendSlice("\\\\"),
            '\n' => try output.appendSlice("\\n"),
            '\r' => try output.appendSlice("\\r"),
            '\t' => try output.appendSlice("\\t"),
            else => try output.append(character),
        }
    }
    try output.append('"');
}

fn sendResponse(stream: std.net.Stream, status: []const u8, content_type: []const u8, body: []const u8) !void {
    var header = std.ArrayList(u8).init(std.heap.page_allocator);
    defer header.deinit();
    try header.writer().print(
        "HTTP/1.1 {s}\r\nContent-Type: {s}\r\nContent-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: GET, POST, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type\r\n\r\n",
        .{ status, content_type, body.len },
    );
    try stream.writeAll(header.items);
    try stream.writeAll(body);
}

fn handleConnection(stream: std.net.Stream, state: *SearchState, page: []const u8) void {
    defer stream.close();
    var buffer: [8192]u8 = undefined;
    const amount = stream.read(&buffer) catch return;
    if (amount == 0) return;
    const request = buffer[0..amount];
    const end = std.mem.indexOf(u8, request, "\r\n") orelse return;
    var line = std.mem.splitScalar(u8, request[0..end], ' ');
    const method = line.next() orelse return;
    const path = line.next() orelse return;
    if (std.mem.eql(u8, method, "OPTIONS")) {
        sendResponse(stream, "204 No Content", "text/plain", "") catch {};
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/")) {
        sendResponse(stream, "200 OK", "text/html; charset=utf-8", page) catch {};
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/favicon.ico")) {
        sendResponse(stream, "204 No Content", "image/x-icon", "") catch {};
        return;
    }
    if (std.mem.eql(u8, method, "POST") and std.mem.startsWith(u8, path, "/api/start")) {
        state.start() catch {
            sendResponse(stream, "500 Internal Server Error", "text/plain", "search could not be started") catch {};
            return;
        };
        sendStatus(stream, state);
        return;
    }
    if (std.mem.eql(u8, method, "POST") and std.mem.eql(u8, path, "/api/stop")) {
        state.stop();
        sendStatus(stream, state);
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/api/status")) {
        sendStatus(stream, state);
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/events")) {
        stream.writeAll("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-store\r\nConnection: keep-alive\r\nAccess-Control-Allow-Origin: *\r\n\r\n") catch return;
        while (true) {
            const body = state.snapshot(std.heap.page_allocator) catch return;
            defer std.heap.page_allocator.free(body);
            stream.writeAll("data: ") catch return;
            stream.writeAll(body) catch return;
            stream.writeAll("\n\n") catch return;
            std.time.sleep(std.time.ns_per_s);
        }
    }
    sendResponse(stream, "404 Not Found", "text/plain", "not found") catch {};
}

fn sendStatus(stream: std.net.Stream, state: *SearchState) void {
    const body = state.snapshot(std.heap.page_allocator) catch {
        sendResponse(stream, "500 Internal Server Error", "text/plain", "status unavailable") catch {};
        return;
    };
    defer std.heap.page_allocator.free(body);
    sendResponse(stream, "200 OK", "application/json", body) catch {};
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var state = try SearchState.init(allocator);
    defer state.deinit();
    const page = @embedFile("index.html");
    var server = std.net.StreamServer.init(.{ .reuse_address = true });
    defer server.deinit();
    try server.listen(try std.net.Address.parseIp4("0.0.0.0", 5000));
    while (true) {
        const connection = try server.accept();
        const thread = std.Thread.spawn(.{}, handleConnection, .{ connection.stream, &state, page }) catch {
            connection.stream.close();
            continue;
        };
        thread.detach();
    }
}

test "candidate checks exact ring arithmetic and enumerated coefficient decoding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    var one = try exact.rationalOne(allocator);
    var zero = try exact.rationalZero(allocator);
    var negative_one_code = try exact.BigInt.initSet(allocator, 2);
    var decoded = try exact.rationalFromCode(allocator, negative_one_code);
    try std.testing.expect(exact.rationalEqual(decoded, try exact.rationalFromInt(allocator, -1)));
    one.deinit();
    zero.deinit();
    decoded.deinit();
    negative_one_code.deinit();
}