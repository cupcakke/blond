const std = @import("std");

const Allocator = std.mem.Allocator;
const Index = struct { x: u32, y: u32 };
const Point = struct { x: i128, y: i128 };

const Rat = struct {
    n: i128,
    d: i128,

    fn gcd(a0: i128, b0: i128) i128 {
        var a = if (a0 < 0) -a0 else a0;
        var b = if (b0 < 0) -b0 else b0;
        while (b != 0) {
            const t = @mod(a, b);
            a = b;
            b = t;
        }
        return if (a == 0) 1 else a;
    }

    fn init(n0: i128, d0: i128) Rat {
        if (n0 == 0) return .{ .n = 0, .d = 1 };
        var n = n0;
        var d = d0;
        if (d < 0) {
            n = -n;
            d = -d;
        }
        const g = gcd(n, d);
        return .{ .n = @divTrunc(n, g), .d = @divTrunc(d, g) };
    }

    fn zero() Rat {
        return init(0, 1);
    }

    fn one() Rat {
        return init(1, 1);
    }

    fn neg(a: Rat) Rat {
        return init(-a.n, a.d);
    }

    fn add(a: Rat, b: Rat) Rat {
        return init(a.n * b.d + b.n * a.d, a.d * b.d);
    }

    fn sub(a: Rat, b: Rat) Rat {
        return init(a.n * b.d - b.n * a.d, a.d * b.d);
    }

    fn mul(a: Rat, b: Rat) Rat {
        return init(a.n * b.n, a.d * b.d);
    }

    fn div(a: Rat, b: Rat) Rat {
        return init(a.n * b.d, a.d * b.n);
    }

    fn equal(a: Rat, b: Rat) bool {
        return a.n == b.n and a.d == b.d;
    }

    fn isZero(a: Rat) bool {
        return a.n == 0;
    }

    fn text(a: Rat, allocator: Allocator) ![]u8 {
        if (a.d == 1) return std.fmt.allocPrint(allocator, "{d}", .{a.n});
        return std.fmt.allocPrint(allocator, "{d}/{d}", .{ a.n, a.d });
    }
};

const Complex = struct {
    re: Rat,
    im: Rat,

    fn zero() Complex {
        return .{ .re = Rat.zero(), .im = Rat.zero() };
    }

    fn one() Complex {
        return .{ .re = Rat.one(), .im = Rat.zero() };
    }

    fn neg(a: Complex) Complex {
        return .{ .re = Rat.neg(a.re), .im = Rat.neg(a.im) };
    }

    fn add(a: Complex, b: Complex) Complex {
        return .{ .re = Rat.add(a.re, b.re), .im = Rat.add(a.im, b.im) };
    }

    fn sub(a: Complex, b: Complex) Complex {
        return .{ .re = Rat.sub(a.re, b.re), .im = Rat.sub(a.im, b.im) };
    }

    fn mul(a: Complex, b: Complex) Complex {
        return .{
            .re = Rat.sub(Rat.mul(a.re, b.re), Rat.mul(a.im, b.im)),
            .im = Rat.add(Rat.mul(a.re, b.im), Rat.mul(a.im, b.re)),
        };
    }

    fn div(a: Complex, b: Complex) Complex {
        const den = Rat.add(Rat.mul(b.re, b.re), Rat.mul(b.im, b.im));
        return .{
            .re = Rat.div(Rat.add(Rat.mul(a.re, b.re), Rat.mul(a.im, b.im)), den),
            .im = Rat.div(Rat.sub(Rat.mul(a.im, b.re), Rat.mul(a.re, b.im)), den),
        };
    }

    fn equal(a: Complex, b: Complex) bool {
        return Rat.equal(a.re, b.re) and Rat.equal(a.im, b.im);
    }

    fn isZero(a: Complex) bool {
        return a.re.isZero() and a.im.isZero();
    }

    fn text(a: Complex, allocator: Allocator) ![]u8 {
        if (a.im.isZero()) return a.re.text(allocator);
        const im = try a.im.text(allocator);
        defer allocator.free(im);
        if (a.re.isZero()) return std.fmt.allocPrint(allocator, "{s}i", .{im});
        const re = try a.re.text(allocator);
        defer allocator.free(re);
        return std.fmt.allocPrint(allocator, "{s}+{s}i", .{ re, im });
    }
};

const Term = struct {
    index: Index,
    value: Complex,
};

const Poly = struct {
    allocator: Allocator,
    terms: std.ArrayList(Term),

    fn init(allocator: Allocator) Poly {
        return .{ .allocator = allocator, .terms = std.ArrayList(Term).init(allocator) };
    }

    fn deinit(self: *Poly) void {
        self.terms.deinit();
    }

    fn clone(self: *const Poly) !Poly {
        var result = Poly.init(self.allocator);
        try result.terms.appendSlice(self.terms.items);
        return result;
    }

    fn cloneInto(self: *const Poly, allocator: Allocator) !Poly {
        var result = Poly.init(allocator);
        try result.terms.appendSlice(self.terms.items);
        return result;
    }

    fn addTerm(self: *Poly, index: Index, value: Complex) !void {
        if (value.isZero()) return;
        for (self.terms.items, 0..) |term, i| {
            if (term.index.x == index.x and term.index.y == index.y) {
                const next = Complex.add(term.value, value);
                if (next.isZero()) {
                    _ = self.terms.orderedRemove(i);
                } else {
                    self.terms.items[i].value = next;
                }
                return;
            }
        }
        try self.terms.append(.{ .index = index, .value = value });
    }

    fn constant(self: *const Poly) Complex {
        for (self.terms.items) |term| {
            if (term.index.x == 0 and term.index.y == 0) return term.value;
        }
        return Complex.zero();
    }

    fn maxX(self: *const Poly) u32 {
        var result: u32 = 0;
        for (self.terms.items) |term| result = @max(result, term.index.x);
        return result;
    }

    fn maxY(self: *const Poly) u32 {
        var result: u32 = 0;
        for (self.terms.items) |term| result = @max(result, term.index.y);
        return result;
    }

    fn add(self: *const Poly, other: *const Poly) !Poly {
        var result = Poly.init(self.allocator);
        try result.terms.appendSlice(self.terms.items);
        for (other.terms.items) |term| try result.addTerm(term.index, term.value);
        return result;
    }

    fn sub(self: *const Poly, other: *const Poly) !Poly {
        var result = Poly.init(self.allocator);
        try result.terms.appendSlice(self.terms.items);
        for (other.terms.items) |term| try result.addTerm(term.index, Complex.neg(term.value));
        return result;
    }

    fn scale(self: *const Poly, factor: Complex) !Poly {
        var result = Poly.init(self.allocator);
        for (self.terms.items) |term| try result.addTerm(term.index, Complex.mul(term.value, factor));
        return result;
    }

    fn multiply(self: *const Poly, other: *const Poly) !Poly {
        var result = Poly.init(self.allocator);
        for (self.terms.items) |a| {
            for (other.terms.items) |b| {
                try result.addTerm(.{ .x = a.index.x + b.index.x, .y = a.index.y + b.index.y }, Complex.mul(a.value, b.value));
            }
        }
        return result;
    }

    fn derivativeX(self: *const Poly) !Poly {
        var result = Poly.init(self.allocator);
        for (self.terms.items) |term| {
            if (term.index.x == 0) continue;
            try result.addTerm(.{ .x = term.index.x - 1, .y = term.index.y }, Complex.mul(term.value, .{ .re = Rat.init(term.index.x, 1), .im = Rat.zero() }));
        }
        return result;
    }

    fn derivativeY(self: *const Poly) !Poly {
        var result = Poly.init(self.allocator);
        for (self.terms.items) |term| {
            if (term.index.y == 0) continue;
            try result.addTerm(.{ .x = term.index.x, .y = term.index.y - 1 }, Complex.mul(term.value, .{ .re = Rat.init(term.index.y, 1), .im = Rat.zero() }));
        }
        return result;
    }

    fn evaluate(self: *const Poly, xv: Complex, yv: Complex) Complex {
        var result = Complex.zero();
        for (self.terms.items) |term| {
            var xp = Complex.one();
            var yp = Complex.one();
            var i: u32 = 0;
            while (i < term.index.x) : (i += 1) xp = Complex.mul(xp, xv);
            i = 0;
            while (i < term.index.y) : (i += 1) yp = Complex.mul(yp, yv);
            result = Complex.add(result, Complex.mul(term.value, Complex.mul(xp, yp)));
        }
        return result;
    }

    fn leading(self: *const Poly) ?Term {
        if (self.terms.items.len == 0) return null;
        var result = self.terms.items[0];
        for (self.terms.items[1..]) |term| {
            if (term.index.x > result.index.x or (term.index.x == result.index.x and term.index.y > result.index.y)) result = term;
        }
        return result;
    }

    fn text(self: *const Poly, allocator: Allocator) ![]u8 {
        var out = std.ArrayList(u8).init(allocator);
        if (self.terms.items.len == 0) {
            try out.append('0');
            return out.toOwnedSlice();
        }
        var sorted = try allocator.alloc(Term, self.terms.items.len);
        defer allocator.free(sorted);
        std.mem.copyForwards(Term, sorted, self.terms.items);
        std.sort.heap(Term, sorted, {}, struct {
            fn lessThan(_: void, a: Term, b: Term) bool {
                return a.index.x > b.index.x or (a.index.x == b.index.x and a.index.y > b.index.y);
            }
        }.lessThan);
        for (sorted, 0..) |term, i| {
            if (i != 0) try out.appendSlice(" + ");
            const coefficient = try term.value.text(allocator);
            defer allocator.free(coefficient);
            const has_monomial = term.index.x != 0 or term.index.y != 0;
            if (!has_monomial) {
                try out.appendSlice(coefficient);
            } else if (Complex.equal(term.value, Complex.one())) {
                try appendMonomial(&out, term.index);
            } else if (Complex.equal(term.value, Complex.neg(Complex.one()))) {
                try out.append('-');
                try appendMonomial(&out, term.index);
            } else {
                try out.append('(');
                try out.appendSlice(coefficient);
                try out.appendSlice(")*");
                try appendMonomial(&out, term.index);
            }
        }
        return out.toOwnedSlice();
    }

    fn appendMonomial(out: *std.ArrayList(u8), index: Index) !void {
        if (index.x != 0) {
            try out.append('x');
            if (index.x != 1) try out.writer().print("^{d}", .{index.x});
        }
        if (index.y != 0) {
            try out.append('y');
            if (index.y != 1) try out.writer().print("^{d}", .{index.y});
        }
    }
};

fn constantPoly(allocator: Allocator, value: Complex) !Poly {
    var result = Poly.init(allocator);
    try result.addTerm(.{ .x = 0, .y = 0 }, value);
    return result;
}

fn jacobian(p: *const Poly, q: *const Poly) !Poly {
    var px = try p.derivativeX();
    var py = try p.derivativeY();
    var qx = try q.derivativeX();
    var qy = try q.derivativeY();
    var left = try px.multiply(&qy);
    var right = try py.multiply(&qx);
    var result = try left.sub(&right);
    try result.addTerm(.{ .x = 0, .y = 0 }, Complex.neg(Complex.one()));
    return result;
}

fn allZero(poly: *const Poly) bool {
    return poly.terms.items.len == 0;
}

fn integerComplex(value: i128) Complex {
    return .{ .re = Rat.init(value, 1), .im = Rat.zero() };
}

fn decodeCoefficient(value: u128, height: u64) Complex {
    const width: u128 = @as(u128, height) * 2 + 1;
    const re = @as(i128, @intCast(value % width)) - @as(i128, @intCast(height));
    const im = @as(i128, @intCast((value / width) % width)) - @as(i128, @intCast(height));
    return .{ .re = Rat.init(re, 1), .im = Rat.init(im, 1) };
}

fn monomialCount(degree: u64) u128 {
    return @as(u128, degree + 1) * @as(u128, degree + 2) / 2;
}

fn powU128(base: u128, exponent: u128) u128 {
    var result: u128 = 1;
    var i: u128 = 0;
    while (i < exponent) : (i += 1) {
        if (result > std.math.maxInt(u128) / base) return std.math.maxInt(u128);
        result *= base;
    }
    return result;
}

fn candidateCount(degree: u64, height: u64) u128 {
    const alphabet = @as(u128, height) * 2 + 1;
    const coefficient_values = alphabet * alphabet;
    const terms = monomialCount(degree);
    return powU128(coefficient_values, terms * 2);
}

fn buildCandidate(allocator: Allocator, degree: u64, height: u64, ordinal: u128) !struct { p: Poly, q: Poly, actual_degree: u64, actual_height: u64 } {
    var p = Poly.init(allocator);
    var q = Poly.init(allocator);
    const base: u128 = (@as(u128, height) * 2 + 1) * (@as(u128, height) * 2 + 1);
    var value = ordinal;
    var actual_degree: u64 = 0;
    var actual_height: u64 = 0;
    var total: u64 = 0;
    while (total <= degree) : (total += 1) {
        var ix: u64 = total + 1;
        while (ix > 0) {
            ix -= 1;
            const iy = total - ix;
            const coefficient = decodeCoefficient(value % base, height);
            value /= base;
            if (!coefficient.isZero()) {
                actual_degree = @max(actual_degree, total);
                actual_height = @max(actual_height, @as(u64, @intCast(@max(@as(i128, @intCast(if (coefficient.re.n < 0) -coefficient.re.n else coefficient.re.n)), @as(i128, @intCast(if (coefficient.im.n < 0) -coefficient.im.n else coefficient.im.n))))));
            }
            try p.addTerm(.{ .x = @intCast(ix), .y = @intCast(iy) }, coefficient);
        }
    }
    total = 0;
    while (total <= degree) : (total += 1) {
        var ix: u64 = total + 1;
        while (ix > 0) {
            ix -= 1;
            const iy = total - ix;
            const coefficient = decodeCoefficient(value % base, height);
            value /= base;
            if (!coefficient.isZero()) {
                actual_degree = @max(actual_degree, total);
                actual_height = @max(actual_height, @as(u64, @intCast(@max(@as(i128, @intCast(if (coefficient.re.n < 0) -coefficient.re.n else coefficient.re.n)), @as(i128, @intCast(if (coefficient.im.n < 0) -coefficient.im.n else coefficient.im.n))))));
            }
            try q.addTerm(.{ .x = @intCast(ix), .y = @intCast(iy) }, coefficient);
        }
    }
    return .{ .p = p, .q = q, .actual_degree = actual_degree, .actual_height = actual_height };
}

const Investigation = struct {
    allocator: Allocator,
    p: Poly,
    q: Poly,
    stage: u64,
    local: u128,
    radius: u64,
    first: u128,
    algebraic: []u8,

    fn deinit(self: *Investigation) void {
        self.p.deinit();
        self.q.deinit();
        self.allocator.free(self.algebraic);
        self.allocator.destroy(self);
    }
};

const SearchState = struct {
    allocator: Allocator,
    mutex: std.Thread.Mutex = .{},
    running: bool = false,
    stopping: bool = false,
    stage: u64 = 0,
    local: u128 = 0,
    tested: u64 = 0,
    jacobian_one: u64 = 0,
    collisions: u64 = 0,
    workers: u32 = 0,
    next_investigation: usize = 0,
    investigations: std.ArrayList(*Investigation),
    latest: []u8,
    started_ns: i128 = 0,
    thread_count: u32 = 0,
    threads: [16]?std.Thread = [_]?std.Thread{null} ** 16,

    fn init(allocator: Allocator) !SearchState {
        return .{
            .allocator = allocator,
            .investigations = std.ArrayList(*Investigation).init(allocator),
            .latest = try allocator.dupe(u8, "{}"),
        };
    }

    fn deinit(self: *SearchState) void {
        self.stop();
        for (self.investigations.items) |job| job.deinit();
        self.investigations.deinit();
        self.allocator.free(self.latest);
    }

    fn stop(self: *SearchState) void {
        self.mutex.lock();
        self.running = false;
        self.stopping = true;
        const count = self.thread_count;
        self.thread_count = 0;
        self.mutex.unlock();
        var i: u32 = 0;
        while (i < count) : (i += 1) {
            if (self.threads[i]) |thread| thread.join();
            self.threads[i] = null;
        }
    }

    fn isRunning(self: *SearchState) bool {
        self.mutex.lock();
        defer self.mutex.unlock();
        return self.running;
    }

    fn setLatest(self: *SearchState, value: []u8) void {
        self.mutex.lock();
        self.allocator.free(self.latest);
        self.latest = value;
        self.mutex.unlock();
    }

    fn snapshot(self: *SearchState, writer: anytype) !void {
        self.mutex.lock();
        defer self.mutex.unlock();
        const elapsed: u64 = if (self.started_ns == 0) 0 else @as(u64, @intCast(@divTrunc(std.time.nanoTimestamp() - self.started_ns, std.time.ns_per_s)));
        try writer.print("{{\"running\":{},\"stage\":{},\"local\":\"{}\",\"tested\":{},\"jacobianOne\":{},\"collisions\":{},\"workers\":{},\"elapsedSeconds\":{},\"latest\":{s}}}", .{
            self.running,
            self.stage,
            self.local,
            self.tested,
            self.jacobian_one,
            self.collisions,
            self.workers,
            elapsed,
            self.latest,
        });
    }

    fn nextWork(self: *SearchState) struct { stage: u64, local: u128 } {
        self.mutex.lock();
        defer self.mutex.unlock();
        const degree = self.stage / 2;
        const height = self.stage - degree;
        const count = candidateCount(degree, height);
        if (self.local >= count) {
            self.stage += 1;
            self.local = 0;
        }
        const result = .{ .stage = self.stage, .local = self.local };
        self.local += 1;
        return result;
    }

    fn register(self: *SearchState, stage: u64, local: u128, p: Poly, q: Poly, algebraic: []u8) !void {
        const job = try self.allocator.create(Investigation);
        job.* = .{ .allocator = self.allocator, .p = p, .q = q, .stage = stage, .local = local, .radius = 0, .first = 0, .algebraic = algebraic };
        self.mutex.lock();
        defer self.mutex.unlock();
        try self.investigations.append(job);
    }

    fn takeInvestigation(self: *SearchState) ?*Investigation {
        self.mutex.lock();
        defer self.mutex.unlock();
        if (self.investigations.items.len == 0) return null;
        const job = self.investigations.items[self.next_investigation % self.investigations.items.len];
        self.next_investigation = (self.next_investigation + 1) % self.investigations.items.len;
        return job;
    }
};

fn pointAt(index: u128) Point {
    if (index == 0) return .{ .x = 0, .y = 0 };
    var radius: u128 = 1;
    var previous: u128 = 1;
    while (true) : (radius += 1) {
        const side = radius * 2 + 1;
        const count = side * side - previous;
        if (index < previous + count) {
            const offset = index - previous;
            const span = radius * 2;
            if (offset < span) return .{ .x = @intCast(-@as(i128, @intCast(radius)) + @as(i128, @intCast(offset))), .y = -@as(i128, @intCast(radius)) };
            if (offset < span * 2) return .{ .x = @as(i128, @intCast(radius)), .y = -@as(i128, @intCast(radius)) + @as(i128, @intCast(offset - span)) };
            if (offset < span * 3) return .{ .x = @as(i128, @intCast(radius)) - @as(i128, @intCast(offset - span * 2)), .y = @as(i128, @intCast(radius)) };
            return .{ .x = -@as(i128, @intCast(radius)), .y = @as(i128, @intCast(radius)) - @as(i128, @intCast(offset - span * 3)) };
        }
        previous += count;
    }
}

fn pointComplex(value: i128) Complex {
    return .{ .re = Rat.init(value, 1), .im = Rat.zero() };
}

fn verifyPointPair(p: *const Poly, q: *const Poly, a: Point, b: Point) bool {
    if (a.x == b.x and a.y == b.y) return false;
    const pa = p.evaluate(pointComplex(a.x), pointComplex(a.y));
    const pb = p.evaluate(pointComplex(b.x), pointComplex(b.y));
    const qa = q.evaluate(pointComplex(a.x), pointComplex(a.y));
    const qb = q.evaluate(pointComplex(b.x), pointComplex(b.y));
    return Complex.equal(pa, pb) and Complex.equal(qa, qb);
}

fn quotientRemainder(f: *const Poly, g: *const Poly) !struct { quotient: Poly, remainder: Poly } {
    var remainder = try f.clone();
    var quotient = Poly.init(f.allocator);
    while (remainder.leading()) |lead| {
        const divisor = g.leading() orelse break;
        if (lead.index.x < divisor.index.x or lead.index.y < divisor.index.y) break;
        const factor = Complex.div(lead.value, divisor.value);
        const delta = Index{ .x = lead.index.x - divisor.index.x, .y = lead.index.y - divisor.index.y };
        try quotient.addTerm(delta, factor);
        var term = Poly.init(f.allocator);
        try term.addTerm(delta, factor);
        var product = try g.multiply(&term);
        var next = try remainder.sub(&product);
        remainder.deinit();
        remainder = next;
    }
    return .{ .quotient = quotient, .remainder = remainder };
}

fn monomialDivides(divisor: Index, target: Index) bool {
    return divisor.x <= target.x and divisor.y <= target.y;
}

fn reduceByBasis(allocator: Allocator, input: *const Poly, basis: []const Poly) !Poly {
    var current = try input.cloneInto(allocator);
    var remainder = Poly.init(allocator);
    while (current.leading()) |leading| {
        var reduced = false;
        for (basis) |*basis_poly| {
            const basis_leading = basis_poly.leading() orelse continue;
            if (!monomialDivides(basis_leading.index, leading.index)) continue;
            const factor = Complex.div(leading.value, basis_leading.value);
            var multiplier = Poly.init(allocator);
            try multiplier.addTerm(.{
                .x = leading.index.x - basis_leading.index.x,
                .y = leading.index.y - basis_leading.index.y,
            }, factor);
            var product = try basis_poly.multiply(&multiplier);
            var next = try current.sub(&product);
            current.deinit();
            current = next;
            reduced = true;
            break;
        }
        if (!reduced) {
            try remainder.addTerm(leading.index, leading.value);
            var singleton = Poly.init(allocator);
            try singleton.addTerm(leading.index, leading.value);
            var next = try current.sub(&singleton);
            current.deinit();
            current = next;
        }
    }
    return remainder;
}

fn groebnerBasis(allocator: Allocator, first: *const Poly, second: *const Poly) !std.ArrayList(Poly) {
    var basis = std.ArrayList(Poly).init(allocator);
    try basis.append(try first.cloneInto(allocator));
    try basis.append(try second.cloneInto(allocator));
    var i: usize = 0;
    while (i < basis.items.len) : (i += 1) {
        var k = i + 1;
        while (k < basis.items.len) : (k += 1) {
            const left = basis.items[i].leading() orelse continue;
            const right = basis.items[k].leading() orelse continue;
            const lcm = Index{
                .x = @max(left.index.x, right.index.x),
                .y = @max(left.index.y, right.index.y),
            };
            var left_multiplier = Poly.init(allocator);
            var right_multiplier = Poly.init(allocator);
            try left_multiplier.addTerm(.{
                .x = lcm.x - left.index.x,
                .y = lcm.y - left.index.y,
            }, Complex.div(Complex.one(), left.value));
            try right_multiplier.addTerm(.{
                .x = lcm.x - right.index.x,
                .y = lcm.y - right.index.y,
            }, Complex.div(Complex.one(), right.value));
            var left_product = try basis.items[i].multiply(&left_multiplier);
            var right_product = try basis.items[k].multiply(&right_multiplier);
            var s_polynomial = try left_product.sub(&right_product);
            var reduced = try reduceByBasis(allocator, &s_polynomial, basis.items);
            if (!allZero(&reduced)) try basis.append(reduced);
        }
    }
    return basis;
}

fn basisText(allocator: Allocator, basis: []const Poly) ![]u8 {
    var out = std.ArrayList(u8).init(allocator);
    try out.append('[');
    for (basis, 0..) |*poly, i| {
        if (i != 0) try out.appendSlice(", ");
        const value = try poly.text(allocator);
        defer allocator.free(value);
        try out.appendSlice(value);
    }
    try out.append(']');
    return out.toOwnedSlice();
}

fn coefficientY(allocator: Allocator, poly: *const Poly, degree: u32) !Poly {
    var result = Poly.init(allocator);
    for (poly.terms.items) |term| {
        if (term.index.y == degree) try result.addTerm(.{ .x = term.index.x, .y = 0 }, term.value);
    }
    return result;
}

fn polyPower(poly: *const Poly, exponent: u32) !Poly {
    var result = try constantPoly(poly.allocator, Complex.one());
    var i: u32 = 0;
    while (i < exponent) : (i += 1) {
        var next = try result.multiply(poly);
        result.deinit();
        result = next;
    }
    return result;
}

fn determinant(allocator: Allocator, matrix: []const Poly, size: usize) !Poly {
    if (size == 0) return constantPoly(allocator, Complex.one());
    if (size == 1) return matrix[0].cloneInto(allocator);
    var result = Poly.init(allocator);
    var column: usize = 0;
    while (column < size) : (column += 1) {
        const minor_size = size - 1;
        var minor = try allocator.alloc(Poly, minor_size * minor_size);
        var minor_row: usize = 0;
        var source_row: usize = 1;
        while (source_row < size) : (source_row += 1) {
            var minor_column: usize = 0;
            var source_column: usize = 0;
            while (source_column < size) : (source_column += 1) {
                if (source_column == column) continue;
                minor[minor_row * minor_size + minor_column] = try matrix[source_row * size + source_column].cloneInto(allocator);
                minor_column += 1;
            }
            minor_row += 1;
        }
        var minor_determinant = try determinant(allocator, minor, minor_size);
        var term = try matrix[column].multiply(&minor_determinant);
        if (column % 2 == 1) {
            var negative = try term.scale(Complex.neg(Complex.one()));
            term.deinit();
            term = negative;
        }
        var next = try result.add(&term);
        result.deinit();
        result = next;
    }
    return result;
}

fn resultantY(allocator: Allocator, first: *const Poly, second: *const Poly) !Poly {
    const first_degree = first.maxY();
    const second_degree = second.maxY();
    if (first_degree == 0) {
        var power = try polyPower(first, second_degree);
        return power;
    }
    if (second_degree == 0) {
        var power = try polyPower(second, first_degree);
        return power;
    }
    const size = @as(usize, first_degree + second_degree);
    var matrix = try allocator.alloc(Poly, size * size);
    for (matrix) |*entry| entry.* = Poly.init(allocator);
    var row: u32 = 0;
    while (row < second_degree) : (row += 1) {
        var degree: u32 = 0;
        while (degree <= first_degree) : (degree += 1) {
            matrix[@as(usize, row) * size + row + degree] = try coefficientY(allocator, first, degree);
        }
    }
    row = 0;
    while (row < first_degree) : (row += 1) {
        var degree: u32 = 0;
        while (degree <= second_degree) : (degree += 1) {
            matrix[@as(usize, second_degree + row) * size + row + degree] = try coefficientY(allocator, second, degree);
        }
    }
    return determinant(allocator, matrix, size);
}

fn algebraicCertificate(allocator: Allocator, p: *const Poly, q: *const Poly) ![]u8 {
    var p0 = try constantPoly(allocator, p.constant());
    var q0 = try constantPoly(allocator, q.constant());
    var first = try p.sub(&p0);
    var second = try q.sub(&q0);
    var basis = try groebnerBasis(allocator, &first, &second);
    const basis_string = try basisText(allocator, basis.items);
    defer allocator.free(basis_string);
    var resultant = try resultantY(allocator, &first, &second);
    const resultant_string = try resultant.text(allocator);
    defer allocator.free(resultant_string);
    var result = std.ArrayList(u8).init(allocator);
    try result.writer().print("Groebner basis for P(x,y)-P(0,0), Q(x,y)-Q(0,0): {s}; Resultant_y={s}", .{ basis_string, resultant_string });
    return result.toOwnedSlice();
}

fn candidateJson(state: *SearchState, stage: u64, local: u128, p: *const Poly, q: *const Poly, j_minus_one: *const Poly, algebraic: []const u8, collision: []const u8) ![]u8 {
    const text_p = try p.text(state.allocator);
    defer state.allocator.free(text_p);
    const text_q = try q.text(state.allocator);
    defer state.allocator.free(text_q);
    var px = try p.derivativeX();
    var py = try p.derivativeY();
    var qx = try q.derivativeX();
    var qy = try q.derivativeY();
    const text_px = try px.text(state.allocator);
    defer state.allocator.free(text_px);
    const text_py = try py.text(state.allocator);
    defer state.allocator.free(text_py);
    const text_qx = try qx.text(state.allocator);
    defer state.allocator.free(text_qx);
    const text_qy = try qy.text(state.allocator);
    defer state.allocator.free(text_qy);
    const text_jm = try j_minus_one.text(state.allocator);
    defer state.allocator.free(text_jm);
    var actual_j = try j_minus_one.cloneInto(state.allocator);
    defer actual_j.deinit();
    try actual_j.addTerm(.{ .x = 0, .y = 0 }, Complex.one());
    const text_j = try actual_j.text(state.allocator);
    defer state.allocator.free(text_j);
    const cert = try std.fmt.allocPrint(state.allocator, "{s}", .{algebraic});
    defer state.allocator.free(cert);
    return std.fmt.allocPrint(state.allocator, "{{\"stage\":{},\"local\":\"{}\",\"P\":\"{s}\",\"Q\":\"{s}\",\"Px\":\"{s}\",\"Py\":\"{s}\",\"Qx\":\"{s}\",\"Qy\":\"{s}\",\"JminusOne\":\"{s}\",\"J\":\"{s}\",\"collision\":{s},\"certificate\":\"{s}\"}}", .{ stage, local, text_p, text_q, text_px, text_py, text_qx, text_qy, text_jm, text_j, if (collision.len == 0) "null" else collision, cert });
}

fn updateCandidate(state: *SearchState, stage: u64, local: u128, p: *const Poly, q: *const Poly, j: *const Poly, algebraic: []const u8) !void {
    state.setLatest(try candidateJson(state, stage, local, p, q, j, algebraic, ""));
}

fn investigate(state: *SearchState, job: *Investigation) !void {
    const r = job.radius;
    const side = r * 2 + 1;
    const count = @as(u128, side) * @as(u128, side);
    if (job.first >= count * count) {
        job.radius += 1;
        job.first = 0;
        return;
    }
    const left_index = job.first / count;
    const right_index = job.first % count;
    job.first += 1;
    const a = pointAt(left_index);
    const b = pointAt(right_index);
    if (!verifyPointPair(&job.p, &job.q, a, b)) return;
    const pt = try std.fmt.allocPrint(state.allocator, "{{\"x1\":\"{}\",\"y1\":\"{}\",\"x2\":\"{}\",\"y2\":\"{}\"}}", .{ a.x, a.y, b.x, b.y });
    state.mutex.lock();
    state.collisions += 1;
    state.mutex.unlock();
    var zero = Poly.init(state.allocator);
    state.setLatest(try candidateJson(state, job.stage, job.local, &job.p, &job.q, &zero, job.algebraic, pt));
    zero.deinit();
}

fn worker(state: *SearchState) void {
    while (state.isRunning()) {
        if (state.takeInvestigation()) |job| {
            investigate(state, job) catch {};
            continue;
        }
        const work = state.nextWork();
        var arena = std.heap.ArenaAllocator.init(state.allocator);
        const allocator = arena.allocator();
        const degree = work.stage / 2;
        const height = work.stage - degree;
        var candidate = buildCandidate(allocator, degree, height, work.local) catch {
            arena.deinit();
            continue;
        };
        defer arena.deinit();
        if (candidate.actual_degree != degree or candidate.actual_height != height) continue;
        var j = jacobian(&candidate.p, &candidate.q) catch continue;
        state.mutex.lock();
        state.tested += 1;
        state.mutex.unlock();
        if (!allZero(&j)) continue;
        state.mutex.lock();
        state.jacobian_one += 1;
        state.mutex.unlock();
        const certificate = algebraicCertificate(allocator, &candidate.p, &candidate.q) catch continue;
        updateCandidate(state, work.stage, work.local, &candidate.p, &candidate.q, &j, certificate) catch {};
        const stable_p = candidate.p.cloneInto(state.allocator) catch continue;
        const stable_q = candidate.q.cloneInto(state.allocator) catch {
            var pp = stable_p;
            pp.deinit();
            continue;
        };
        const stable_certificate = state.allocator.dupe(u8, certificate) catch continue;
        state.register(work.stage, work.local, stable_p, stable_q, stable_certificate) catch {
            var pp = stable_p;
            var qq = stable_q;
            pp.deinit();
            qq.deinit();
            state.allocator.free(stable_certificate);
        };
    }
}

fn jsonEscape(allocator: Allocator, input: []const u8) ![]u8 {
    var out = std.ArrayList(u8).init(allocator);
    for (input) |c| {
        switch (c) {
            '"' => try out.appendSlice("\\\""),
            '\\' => try out.appendSlice("\\\\"),
            '\n' => try out.appendSlice("\\n"),
            '\r' => try out.appendSlice("\\r"),
            '\t' => try out.appendSlice("\\t"),
            else => try out.append(c),
        }
    }
    return out.toOwnedSlice();
}

fn sendResponse(stream: std.net.Stream, status: []const u8, content_type: []const u8, body: []const u8) !void {
    var header = std.ArrayList(u8).init(std.heap.page_allocator);
    defer header.deinit();
    try header.writer().print("HTTP/1.1 {s}\r\nContent-Type: {s}\r\nContent-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\nAccess-Control-Allow-Origin: *\r\n\r\n", .{ status, content_type, body.len });
    try stream.writeAll(header.items);
    try stream.writeAll(body);
}

fn statusBody(state: *SearchState, allocator: Allocator) ![]u8 {
    var out = std.ArrayList(u8).init(allocator);
    try state.snapshot(out.writer());
    return out.toOwnedSlice();
}

fn parseWorkers(path: []const u8) u32 {
    const marker = "workers=";
    const start = std.mem.indexOf(u8, path, marker) orelse return 4;
    var value: u32 = 0;
    var i = start + marker.len;
    while (i < path.len and path[i] >= '0' and path[i] <= '9') : (i += 1) value = value * 10 + path[i] - '0';
    return std.math.clamp(value, 1, 16);
}

fn startSearch(state: *SearchState, workers: u32) !void {
    state.mutex.lock();
    if (state.running) {
        state.mutex.unlock();
        return;
    }
    state.running = true;
    state.stopping = false;
    state.workers = workers;
    state.started_ns = std.time.nanoTimestamp();
    state.mutex.unlock();
    var i: u32 = 0;
    while (i < workers) : (i += 1) {
        const thread = std.Thread.spawn(.{}, worker, .{state}) catch {
            state.stop();
            return error.ThreadSpawnFailed;
        };
        state.mutex.lock();
        state.threads[i] = thread;
        state.thread_count = i + 1;
        state.mutex.unlock();
    }
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
    if (std.mem.eql(u8, method, "GET") and (std.mem.eql(u8, path, "/") or std.mem.startsWith(u8, path, "/index.html"))) {
        sendResponse(stream, "200 OK", "text/html; charset=utf-8", page) catch {};
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/favicon.ico")) {
        sendResponse(stream, "204 No Content", "image/x-icon", "") catch {};
        return;
    }
    if (std.mem.eql(u8, method, "POST") and std.mem.startsWith(u8, path, "/api/start")) {
        startSearch(state, parseWorkers(path)) catch {};
        const body = statusBody(state, std.heap.page_allocator) catch return;
        defer std.heap.page_allocator.free(body);
        sendResponse(stream, "200 OK", "application/json", body) catch {};
        return;
    }
    if (std.mem.eql(u8, method, "POST") and std.mem.eql(u8, path, "/api/stop")) {
        state.stop();
        const body = statusBody(state, std.heap.page_allocator) catch return;
        defer std.heap.page_allocator.free(body);
        sendResponse(stream, "200 OK", "application/json", body) catch {};
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/api/status")) {
        const body = statusBody(state, std.heap.page_allocator) catch return;
        defer std.heap.page_allocator.free(body);
        sendResponse(stream, "200 OK", "application/json", body) catch {};
        return;
    }
    if (std.mem.eql(u8, method, "GET") and std.mem.eql(u8, path, "/events")) {
        stream.writeAll("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-store\r\nConnection: keep-alive\r\nAccess-Control-Allow-Origin: *\r\n\r\n") catch return;
        while (state.isRunning()) {
            const body = statusBody(state, std.heap.page_allocator) catch return;
            defer std.heap.page_allocator.free(body);
            stream.writeAll("data: ") catch return;
            stream.writeAll(body) catch return;
            stream.writeAll("\n\n") catch return;
            std.time.sleep(500 * std.time.ns_per_ms);
        }
        const body = statusBody(state, std.heap.page_allocator) catch return;
        defer std.heap.page_allocator.free(body);
        stream.writeAll("data: ") catch return;
        stream.writeAll(body) catch return;
        stream.writeAll("\n\n") catch {};
        return;
    }
    sendResponse(stream, "404 Not Found", "text/plain", "not found") catch {};
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
        _ = std.Thread.spawn(.{}, handleConnection, .{ connection.stream, &state, page }) catch connection.stream.close();
    }
}
