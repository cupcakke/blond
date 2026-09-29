const std = @import("std");
const exact = @import("exact.zig");

const Allocator = std.mem.Allocator;
const BigInt = exact.BigInt;

pub const Work = struct {
    stage: []u8,
    global_index: []u8,
    local_index: []u8,
    degree: usize,
    modulus_degree: usize,
    codes: []BigInt,
};

pub const Iterator = struct {
    allocator: Allocator,
    stage: BigInt,
    degree: BigInt,
    modulus_degree: BigInt,
    global_index: BigInt,
    local_index: BigInt,
    coefficient_sum: BigInt,
    composition_count: BigInt,
    coefficients: []BigInt,

    pub fn init(allocator: Allocator) !Iterator {
        var stage = try BigInt.initSet(allocator, 0);
        errdefer stage.deinit();
        var degree = try BigInt.initSet(allocator, 0);
        errdefer degree.deinit();
        var modulus_degree = try BigInt.initSet(allocator, 1);
        errdefer modulus_degree.deinit();
        var global_index = try BigInt.initSet(allocator, 0);
        errdefer global_index.deinit();
        var local_index = try BigInt.initSet(allocator, 0);
        errdefer local_index.deinit();
        var coefficient_sum = try BigInt.initSet(allocator, 0);
        errdefer coefficient_sum.deinit();
        var composition_count = try BigInt.initSet(allocator, 1);
        errdefer composition_count.deinit();
        var result = Iterator{
            .allocator = allocator,
            .stage = stage,
            .degree = degree,
            .modulus_degree = modulus_degree,
            .global_index = global_index,
            .local_index = local_index,
            .coefficient_sum = coefficient_sum,
            .composition_count = composition_count,
            .coefficients = &.{},
        };
        errdefer result.releaseCoefficients();
        try result.resetComposition();
        return result;
    }

    pub fn deinit(self: *Iterator) void {
        self.releaseCoefficients();
        self.stage.deinit();
        self.degree.deinit();
        self.modulus_degree.deinit();
        self.global_index.deinit();
        self.local_index.deinit();
        self.coefficient_sum.deinit();
        self.composition_count.deinit();
    }

    pub fn next(self: *Iterator, allocator: Allocator) !Work {
        const degree = self.degree.to(usize) catch return error.ResourceLimit;
        const modulus_degree = self.modulus_degree.to(usize) catch return error.ResourceLimit;
        const codes = try allocator.alloc(BigInt, self.coefficients.len);
        for (codes, 0..) |*destination, i| {
            destination.* = try BigInt.init(allocator);
            try destination.copy(self.coefficients[i].toConst());
        }
        const result = Work{
            .stage = try self.stage.toString(allocator, 10, .lower),
            .global_index = try self.global_index.toString(allocator, 10, .lower),
            .local_index = try self.local_index.toString(allocator, 10, .lower),
            .degree = degree,
            .modulus_degree = modulus_degree,
            .codes = codes,
        };
        try BigInt.addScalar(&self.global_index, &self.global_index, 1);
        try BigInt.addScalar(&self.local_index, &self.local_index, 1);
        if (!try self.nextComposition()) try self.advanceShell();
        return result;
    }

    fn releaseCoefficients(self: *Iterator) void {
        for (self.coefficients) |*coefficient| coefficient.deinit();
        if (self.coefficients.len > 0) self.allocator.free(self.coefficients);
        self.coefficients = &.{};
    }

    fn resetComposition(self: *Iterator) !void {
        var dimension_big = try self.dimensionBig();
        defer dimension_big.deinit();
        const dimension = dimension_big.to(usize) catch return error.ResourceLimit;
        const coefficients = try self.allocator.alloc(BigInt, dimension);
        var initialized: usize = 0;
        var committed = false;
        errdefer if (!committed) {
            for (coefficients[0..initialized]) |*coefficient| coefficient.deinit();
            self.allocator.free(coefficients);
        };
        for (coefficients) |*coefficient| {
            coefficient.* = try BigInt.initSet(self.allocator, 0);
            initialized += 1;
        }
        var coefficient_sum = try subtract(self.allocator, self.stage, self.degree);
        defer coefficient_sum.deinit();
        var m_minus_one = try clone(self.allocator, self.modulus_degree);
        defer m_minus_one.deinit();
        var one = try BigInt.initSet(self.allocator, 1);
        defer one.deinit();
        try BigInt.sub(&m_minus_one, &m_minus_one, &one);
        try BigInt.sub(&coefficient_sum, &coefficient_sum, &m_minus_one);
        try coefficients[dimension - 1].copy(coefficient_sum.toConst());
        var composition_count = try weakCompositionCount(self.allocator, dimension, coefficient_sum);
        self.releaseCoefficients();
        self.coefficients = coefficients;
        committed = true;
        try self.coefficient_sum.copy(coefficient_sum.toConst());
        self.composition_count.deinit();
        self.composition_count = composition_count;
        try self.local_index.set(0);
    }

    fn dimensionBig(self: *const Iterator) !BigInt {
        var degree_plus_one = try clone(self.allocator, self.degree);
        defer degree_plus_one.deinit();
        try BigInt.addScalar(&degree_plus_one, &degree_plus_one, 1);
        var degree_plus_two = try clone(self.allocator, self.degree);
        defer degree_plus_two.deinit();
        try BigInt.addScalar(&degree_plus_two, &degree_plus_two, 2);
        var monomial_product = try BigInt.init(self.allocator);
        defer monomial_product.deinit();
        try BigInt.mul(&monomial_product, &degree_plus_one, &degree_plus_two);
        var two = try BigInt.initSet(self.allocator, 2);
        defer two.deinit();
        var monomial_count = try BigInt.init(self.allocator);
        defer monomial_count.deinit();
        var remainder = try BigInt.init(self.allocator);
        defer remainder.deinit();
        try BigInt.divFloor(&monomial_count, &remainder, &monomial_product, &two);
        var factor = try BigInt.init(self.allocator);
        defer factor.deinit();
        try BigInt.mul(&factor, &monomial_count, &two);
        try BigInt.addScalar(&factor, &factor, 7);
        var dimension = try BigInt.init(self.allocator);
        errdefer dimension.deinit();
        try BigInt.mul(&dimension, &self.modulus_degree, &factor);
        return dimension;
    }

    fn nextComposition(self: *Iterator) !bool {
        if (self.coefficients.len < 2) return false;
        var position = self.coefficients.len - 1;
        while (position > 0) {
            position -= 1;
            var tail = try BigInt.initSet(self.allocator, 0);
            defer tail.deinit();
            var j = position + 1;
            while (j < self.coefficients.len) : (j += 1) {
                try BigInt.add(&tail, &tail, &self.coefficients[j]);
            }
            if (!tail.eqlZero()) {
                try BigInt.addScalar(&self.coefficients[position], &self.coefficients[position], 1);
                j = position + 1;
                while (j < self.coefficients.len - 1) : (j += 1) {
                    try self.coefficients[j].set(0);
                }
                var one = try BigInt.initSet(self.allocator, 1);
                defer one.deinit();
                try BigInt.sub(&self.coefficients[self.coefficients.len - 1], &tail, &one);
                return true;
            }
        }
        return false;
    }

    fn advanceShell(self: *Iterator) !void {
        var stage_minus_degree = try subtract(self.allocator, self.stage, self.degree);
        defer stage_minus_degree.deinit();
        try BigInt.addScalar(&stage_minus_degree, &stage_minus_degree, 1);
        if (BigInt.order(self.modulus_degree, stage_minus_degree) == .lt) {
            try BigInt.addScalar(&self.modulus_degree, &self.modulus_degree, 1);
        } else if (BigInt.order(self.degree, self.stage) == .lt) {
            try BigInt.addScalar(&self.degree, &self.degree, 1);
            try self.modulus_degree.set(1);
        } else {
            try BigInt.addScalar(&self.stage, &self.stage, 1);
            try self.degree.set(0);
            try self.modulus_degree.set(1);
        }
        try self.resetComposition();
    }
};

fn clone(allocator: Allocator, source: BigInt) !BigInt {
    var result = try BigInt.init(allocator);
    try result.copy(source.toConst());
    return result;
}

fn subtract(allocator: Allocator, a: BigInt, b: BigInt) !BigInt {
    var result = try BigInt.init(allocator);
    try BigInt.sub(&result, &a, &b);
    return result;
}

fn weakCompositionCount(allocator: Allocator, length: usize, sum: BigInt) !BigInt {
    if (length == 0) return error.InvalidComposition;
    var dimension_minus_one = try BigInt.initSet(allocator, length - 1);
    defer dimension_minus_one.deinit();
    const k = if (BigInt.order(sum, dimension_minus_one) == .lt)
        try sum.to(usize)
    else
        length - 1;
    var n = try clone(allocator, sum);
    defer n.deinit();
    try BigInt.addScalar(&n, &n, length - 1);
    var n_minus_k = try clone(allocator, n);
    defer n_minus_k.deinit();
    var big_k = try BigInt.initSet(allocator, k);
    defer big_k.deinit();
    try BigInt.sub(&n_minus_k, &n_minus_k, &big_k);
    var result = try BigInt.initSet(allocator, 1);
    errdefer result.deinit();
    var i: usize = 1;
    while (i <= k) : (i += 1) {
        var factor = try clone(allocator, n_minus_k);
        defer factor.deinit();
        try BigInt.addScalar(&factor, &factor, i);
        try BigInt.mul(&result, &result, &factor);
        var divisor = try BigInt.initSet(allocator, i);
        defer divisor.deinit();
        var quotient = try BigInt.init(allocator);
        defer quotient.deinit();
        var remainder = try BigInt.init(allocator);
        defer remainder.deinit();
        try BigInt.divFloor(&quotient, &remainder, &result, &divisor);
        if (!remainder.eqlZero()) return error.InvalidCompositionCount;
        try result.copy(quotient.toConst());
    }
    return result;
}

test "enumeration starts with the first required weak compositions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var iterator = try Iterator.init(arena.allocator());
    defer iterator.deinit();
    const first = try iterator.next(arena.allocator());
    try std.testing.expectEqualStrings("0", first.stage);
    try std.testing.expectEqualStrings("0", first.global_index);
    try std.testing.expectEqual(@as(usize, 0), first.degree);
    try std.testing.expectEqual(@as(usize, 1), first.modulus_degree);
    const second = try iterator.next(arena.allocator());
    try std.testing.expectEqualStrings("1", second.stage);
    try std.testing.expectEqualStrings("1", second.global_index);
    try std.testing.expectEqual(@as(usize, 0), second.degree);
    try std.testing.expectEqual(@as(usize, 1), second.modulus_degree);
}

test "stage, degree, modulus, and lexicographic composition order are complete" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var iterator = try Iterator.init(std.testing.allocator);
    defer iterator.deinit();
    const first = try iterator.next(arena.allocator());
    try std.testing.expectEqual(@as(usize, 9), first.codes.len);
    try std.testing.expect(first.codes[8].eqlZero());
    const stage_one_first = try iterator.next(arena.allocator());
    try std.testing.expectEqualStrings("1", stage_one_first.stage);
    try std.testing.expectEqual(@as(usize, 0), stage_one_first.degree);
    try std.testing.expectEqual(@as(usize, 1), stage_one_first.modulus_degree);
    try std.testing.expectEqual(@as(u64, 1), try stage_one_first.codes[8].to(u64));
    const stage_one_second = try iterator.next(arena.allocator());
    try std.testing.expect(stage_one_second.codes[8].eqlZero());
    try std.testing.expectEqual(@as(u64, 1), try stage_one_second.codes[7].to(u64));
    var index: usize = 2;
    while (index < 9) : (index += 1) _ = try iterator.next(arena.allocator());
    const next_modulus = try iterator.next(arena.allocator());
    try std.testing.expectEqualStrings("1", next_modulus.stage);
    try std.testing.expectEqual(@as(usize, 0), next_modulus.degree);
    try std.testing.expectEqual(@as(usize, 2), next_modulus.modulus_degree);
    try std.testing.expectEqual(@as(usize, 18), next_modulus.codes.len);
    const next_degree = try iterator.next(arena.allocator());
    try std.testing.expectEqual(@as(usize, 1), next_degree.degree);
    try std.testing.expectEqual(@as(usize, 1), next_degree.modulus_degree);
    try std.testing.expectEqual(@as(usize, 13), next_degree.codes.len);
    const next_stage = try iterator.next(arena.allocator());
    try std.testing.expectEqualStrings("2", next_stage.stage);
    try std.testing.expectEqual(@as(usize, 0), next_stage.degree);
    try std.testing.expectEqual(@as(u64, 2), try next_stage.codes[8].to(u64));
}