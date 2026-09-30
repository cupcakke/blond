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

    pub fn deinit(self: *Work, allocator: Allocator) void {
        for (self.codes) |*code| code.deinit();
        allocator.free(self.codes);
        allocator.free(self.stage);
        allocator.free(self.global_index);
        allocator.free(self.local_index);
        self.codes = &.{};
        self.stage = &.{};
        self.global_index = &.{};
        self.local_index = &.{};
    }
};

const Composition = struct {
    coefficient_sum: BigInt,
    composition_count: BigInt,
    local_index: BigInt,
    coefficients: []BigInt,
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
        var result = initial: {
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

            break :initial Iterator{
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
        };
        errdefer result.deinit();

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
        var initialized: usize = 0;
        errdefer {
            for (codes[0..initialized]) |*code| code.deinit();
            allocator.free(codes);
        }

        for (codes, 0..) |*destination, i| {
            destination.* = try BigInt.init(allocator);
            initialized += 1;
            try destination.copy(self.coefficients[i].toConst());
        }

        const stage_text = try self.stage.toString(allocator, 10, .lower);
        errdefer allocator.free(stage_text);

        const global_index_text = try self.global_index.toString(allocator, 10, .lower);
        errdefer allocator.free(global_index_text);

        const local_index_text = try self.local_index.toString(allocator, 10, .lower);
        errdefer allocator.free(local_index_text);

        const result = Work{
            .stage = stage_text,
            .global_index = global_index_text,
            .local_index = local_index_text,
            .degree = degree,
            .modulus_degree = modulus_degree,
            .codes = codes,
        };

        var next_global_index = try clone(self.allocator, self.global_index);
        defer next_global_index.deinit();
        try BigInt.addScalar(&next_global_index, &next_global_index, 1);

        var next_local_index = try clone(self.allocator, self.local_index);
        defer next_local_index.deinit();
        try BigInt.addScalar(&next_local_index, &next_local_index, 1);

        switch (BigInt.order(next_local_index, self.composition_count)) {
            .lt => {
                if (!try self.nextComposition()) return error.InvalidComposition;

                const previous_local_index = self.local_index;
                self.local_index = next_local_index;
                next_local_index = previous_local_index;
            },
            .eq => try self.advanceShell(),
            .gt => return error.InvalidCompositionIndex,
        }

        const previous_global_index = self.global_index;
        self.global_index = next_global_index;
        next_global_index = previous_global_index;

        return result;
    }

    fn releaseCoefficients(self: *Iterator) void {
        for (self.coefficients) |*coefficient| coefficient.deinit();
        if (self.coefficients.len > 0) self.allocator.free(self.coefficients);
        self.coefficients = &.{};
    }

    fn resetComposition(self: *Iterator) !void {
        const composition = try self.prepareComposition(
            self.stage,
            self.degree,
            self.modulus_degree,
        );
        self.installComposition(composition);
    }

    fn prepareComposition(
        self: *const Iterator,
        stage: BigInt,
        degree: BigInt,
        modulus_degree: BigInt,
    ) !Composition {
        var dimension_big = try self.dimensionBig(degree, modulus_degree);
        defer dimension_big.deinit();

        const dimension = dimension_big.to(usize) catch return error.ResourceLimit;
        if (dimension == 0) return error.InvalidComposition;

        const coefficients = try self.allocator.alloc(BigInt, dimension);
        var initialized: usize = 0;
        errdefer {
            for (coefficients[0..initialized]) |*coefficient| coefficient.deinit();
            self.allocator.free(coefficients);
        }

        for (coefficients) |*coefficient| {
            coefficient.* = try BigInt.initSet(self.allocator, 0);
            initialized += 1;
        }

        var coefficient_sum = try subtract(self.allocator, stage, degree);
        errdefer coefficient_sum.deinit();

        var m_minus_one = try clone(self.allocator, modulus_degree);
        defer m_minus_one.deinit();

        var one = try BigInt.initSet(self.allocator, 1);
        defer one.deinit();

        try BigInt.sub(&m_minus_one, &m_minus_one, &one);
        try BigInt.sub(&coefficient_sum, &coefficient_sum, &m_minus_one);

        if (!coefficient_sum.isPositive() and !coefficient_sum.eqlZero()) return error.InvalidComposition;

        try coefficients[dimension - 1].copy(coefficient_sum.toConst());

        var composition_count = try weakCompositionCount(
            self.allocator,
            dimension,
            coefficient_sum,
        );
        errdefer composition_count.deinit();

        var local_index = try BigInt.initSet(self.allocator, 0);
        errdefer local_index.deinit();

        return Composition{
            .coefficient_sum = coefficient_sum,
            .composition_count = composition_count,
            .local_index = local_index,
            .coefficients = coefficients,
        };
    }

    fn installComposition(self: *Iterator, composition: Composition) void {
        self.releaseCoefficients();
        self.coefficient_sum.deinit();
        self.composition_count.deinit();
        self.local_index.deinit();

        self.coefficients = composition.coefficients;
        self.coefficient_sum = composition.coefficient_sum;
        self.composition_count = composition.composition_count;
        self.local_index = composition.local_index;
    }

    fn dimensionBig(
        self: *const Iterator,
        degree: BigInt,
        modulus_degree: BigInt,
    ) !BigInt {
        var degree_plus_one = try clone(self.allocator, degree);
        defer degree_plus_one.deinit();
        try BigInt.addScalar(&degree_plus_one, &degree_plus_one, 1);

        var degree_plus_two = try clone(self.allocator, degree);
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
        try BigInt.mul(&dimension, &modulus_degree, &factor);

        return dimension;
    }

    fn nextComposition(self: *Iterator) !bool {
        if (self.coefficients.len < 2) return false;

        var tail = try clone(self.allocator, self.coefficient_sum);
        defer tail.deinit();

        for (self.coefficients[0 .. self.coefficients.len - 1]) |*coefficient| {
            try BigInt.sub(&tail, &tail, coefficient);
        }

        const last_coefficient = self.coefficients[self.coefficients.len - 1];
        if (!tail.eql(last_coefficient) and !(tail.eqlZero() and last_coefficient.eqlZero())) {
            return error.InvalidComposition;
        }

        var position = self.coefficients.len - 1;
        while (position > 0) {
            position -= 1;

            if (!tail.eqlZero()) {
                const replacements = try self.allocator.alloc(
                    BigInt,
                    self.coefficients.len - position,
                );
                var initialized: usize = 0;
                defer {
                    for (replacements[0..initialized]) |*replacement| replacement.deinit();
                    self.allocator.free(replacements);
                }

                replacements[0] = try clone(
                    self.allocator,
                    self.coefficients[position],
                );
                initialized += 1;
                try BigInt.addScalar(&replacements[0], &replacements[0], 1);

                var replacement_index: usize = 1;
                while (replacement_index < replacements.len - 1) : (replacement_index += 1) {
                    replacements[replacement_index] = try BigInt.initSet(
                        self.allocator,
                        0,
                    );
                    initialized += 1;
                }

                var one = try BigInt.initSet(self.allocator, 1);
                defer one.deinit();

                replacements[replacements.len - 1] = try subtract(
                    self.allocator,
                    tail,
                    one,
                );
                initialized += 1;

                for (self.coefficients[position..], replacements) |*coefficient, *replacement| {
                    const previous = coefficient.*;
                    coefficient.* = replacement.*;
                    replacement.* = previous;
                }

                return true;
            }

            try BigInt.add(&tail, &tail, &self.coefficients[position]);
        }

        return false;
    }

    fn advanceShell(self: *Iterator) !void {
        var stage = try clone(self.allocator, self.stage);
        errdefer stage.deinit();

        var degree = try clone(self.allocator, self.degree);
        errdefer degree.deinit();

        var modulus_degree = try clone(self.allocator, self.modulus_degree);
        errdefer modulus_degree.deinit();

        var stage_minus_degree = try subtract(self.allocator, stage, degree);
        defer stage_minus_degree.deinit();
        try BigInt.addScalar(&stage_minus_degree, &stage_minus_degree, 1);

        if (BigInt.order(modulus_degree, stage_minus_degree) == .lt) {
            try BigInt.addScalar(&modulus_degree, &modulus_degree, 1);
        } else if (BigInt.order(degree, stage) == .lt) {
            try BigInt.addScalar(&degree, &degree, 1);
            try modulus_degree.set(1);
        } else {
            try BigInt.addScalar(&stage, &stage, 1);
            try degree.set(0);
            try modulus_degree.set(1);
        }

        const composition = try self.prepareComposition(
            stage,
            degree,
            modulus_degree,
        );

        self.stage.deinit();
        self.degree.deinit();
        self.modulus_degree.deinit();

        self.stage = stage;
        self.degree = degree;
        self.modulus_degree = modulus_degree;
        self.installComposition(composition);
    }
};

fn clone(allocator: Allocator, source: BigInt) !BigInt {
    var result = try BigInt.init(allocator);
    errdefer result.deinit();
    try result.copy(source.toConst());
    return result;
}

fn subtract(allocator: Allocator, a: BigInt, b: BigInt) !BigInt {
    var result = try BigInt.init(allocator);
    errdefer result.deinit();
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

fn verifyIteratorAllocationFailures(allocator: Allocator) !void {
    var iterator = try Iterator.init(allocator);
    defer iterator.deinit();

    var index: usize = 0;
    while (index < 13) : (index += 1) {
        const previous_stage = try iterator.stage.to(u64);
        const previous_degree = try iterator.degree.to(u64);
        const previous_modulus_degree = try iterator.modulus_degree.to(u64);
        const previous_global_index = try iterator.global_index.to(u64);
        const previous_local_index = try iterator.local_index.to(u64);
        const previous_coefficient_sum = try iterator.coefficient_sum.to(u64);
        const previous_composition_count = try iterator.composition_count.to(u64);

        const previous_coefficients = try std.testing.allocator.alloc(
            u64,
            iterator.coefficients.len,
        );
        defer std.testing.allocator.free(previous_coefficients);

        for (iterator.coefficients, 0..) |coefficient, i| {
            previous_coefficients[i] = try coefficient.to(u64);
        }

        var work = iterator.next(allocator) catch |err| {
            try std.testing.expectEqual(previous_stage, try iterator.stage.to(u64));
            try std.testing.expectEqual(previous_degree, try iterator.degree.to(u64));
            try std.testing.expectEqual(
                previous_modulus_degree,
                try iterator.modulus_degree.to(u64),
            );
            try std.testing.expectEqual(
                previous_global_index,
                try iterator.global_index.to(u64),
            );
            try std.testing.expectEqual(
                previous_local_index,
                try iterator.local_index.to(u64),
            );
            try std.testing.expectEqual(
                previous_coefficient_sum,
                try iterator.coefficient_sum.to(u64),
            );
            try std.testing.expectEqual(
                previous_composition_count,
                try iterator.composition_count.to(u64),
            );
            try std.testing.expectEqual(
                previous_coefficients.len,
                iterator.coefficients.len,
            );

            for (iterator.coefficients, 0..) |coefficient, i| {
                try std.testing.expectEqual(
                    previous_coefficients[i],
                    try coefficient.to(u64),
                );
            }

            return err;
        };
        defer work.deinit(allocator);

        try std.testing.expectEqual(
            index + 1,
            try iterator.global_index.to(usize),
        );
    }
}

test "enumeration starts with the first required weak compositions" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var iterator = try Iterator.init(arena.allocator());
    defer iterator.deinit();

    var first = try iterator.next(arena.allocator());
    defer first.deinit(arena.allocator());

    try std.testing.expectEqualStrings("0", first.stage);
    try std.testing.expectEqualStrings("0", first.global_index);
    try std.testing.expectEqual(@as(usize, 0), first.degree);
    try std.testing.expectEqual(@as(usize, 1), first.modulus_degree);

    var second = try iterator.next(arena.allocator());
    defer second.deinit(arena.allocator());

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

    var first = try iterator.next(arena.allocator());
    defer first.deinit(arena.allocator());

    try std.testing.expectEqual(@as(usize, 9), first.codes.len);
    try std.testing.expect(first.codes[8].eqlZero());

    var stage_one_first = try iterator.next(arena.allocator());
    defer stage_one_first.deinit(arena.allocator());

    try std.testing.expectEqualStrings("1", stage_one_first.stage);
    try std.testing.expectEqual(@as(usize, 0), stage_one_first.degree);
    try std.testing.expectEqual(@as(usize, 1), stage_one_first.modulus_degree);
    try std.testing.expectEqual(@as(u64, 1), try stage_one_first.codes[8].to(u64));

    var stage_one_second = try iterator.next(arena.allocator());
    defer stage_one_second.deinit(arena.allocator());

    try std.testing.expect(stage_one_second.codes[8].eqlZero());
    try std.testing.expectEqual(@as(u64, 1), try stage_one_second.codes[7].to(u64));

    var index: usize = 2;
    while (index < 9) : (index += 1) {
        var work = try iterator.next(arena.allocator());
        work.deinit(arena.allocator());
    }

    var next_modulus = try iterator.next(arena.allocator());
    defer next_modulus.deinit(arena.allocator());

    try std.testing.expectEqualStrings("1", next_modulus.stage);
    try std.testing.expectEqual(@as(usize, 0), next_modulus.degree);
    try std.testing.expectEqual(@as(usize, 2), next_modulus.modulus_degree);
    try std.testing.expectEqual(@as(usize, 18), next_modulus.codes.len);

    var next_degree = try iterator.next(arena.allocator());
    defer next_degree.deinit(arena.allocator());

    try std.testing.expectEqual(@as(usize, 1), next_degree.degree);
    try std.testing.expectEqual(@as(usize, 1), next_degree.modulus_degree);
    try std.testing.expectEqual(@as(usize, 13), next_degree.codes.len);

    var next_stage = try iterator.next(arena.allocator());
    defer next_stage.deinit(arena.allocator());

    try std.testing.expectEqualStrings("2", next_stage.stage);
    try std.testing.expectEqual(@as(usize, 0), next_stage.degree);
    try std.testing.expectEqual(@as(u64, 2), try next_stage.codes[8].to(u64));
}

test "work remains valid after its iterator is deinitialized" {
    var work = owned: {
        var iterator = try Iterator.init(std.testing.allocator);
        defer iterator.deinit();

        break :owned try iterator.next(std.testing.allocator);
    };
    defer work.deinit(std.testing.allocator);

    try std.testing.expectEqualStrings("0", work.stage);
    try std.testing.expectEqualStrings("0", work.global_index);
    try std.testing.expectEqualStrings("0", work.local_index);
    try std.testing.expectEqual(@as(usize, 0), work.degree);
    try std.testing.expectEqual(@as(usize, 1), work.modulus_degree);
    try std.testing.expectEqual(@as(usize, 9), work.codes.len);

    for (work.codes) |code| {
        try std.testing.expect(code.eqlZero());
    }
}

test "weak composition counts are exact" {
    var sum = try BigInt.initSet(std.testing.allocator, 2);
    defer sum.deinit();

    var count = try weakCompositionCount(std.testing.allocator, 9, sum);
    defer count.deinit();
    try std.testing.expectEqual(@as(u64, 45), try count.to(u64));

    var single_dimension_count = try weakCompositionCount(
        std.testing.allocator,
        1,
        sum,
    );
    defer single_dimension_count.deinit();
    try std.testing.expectEqual(
        @as(u64, 1),
        try single_dimension_count.to(u64),
    );

    var zero = try BigInt.initSet(std.testing.allocator, 0);
    defer zero.deinit();

    var zero_sum_count = try weakCompositionCount(
        std.testing.allocator,
        18,
        zero,
    );
    defer zero_sum_count.deinit();
    try std.testing.expectEqual(@as(u64, 1), try zero_sum_count.to(u64));

    try std.testing.expectError(
        error.InvalidComposition,
        weakCompositionCount(std.testing.allocator, 0, zero),
    );
}

test "allocation failure paths release memory without changing iterator state" {
    try std.testing.checkAllAllocationFailures(
        std.testing.allocator,
        verifyIteratorAllocationFailures,
        .{},
    );
}