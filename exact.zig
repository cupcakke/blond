const std = @import("std");

pub const Allocator = std.mem.Allocator;
pub const Rational = std.math.big.Rational;
pub const BigInt = std.math.big.int.Managed;

pub const Index = struct {
    x: usize,
    y: usize,
};

pub const Element = struct {
    values: []Rational,
};

pub const Term = struct {
    index: Index,
    value: Element,
};

pub const Polynomial = struct {
    allocator: Allocator,
    modulus: []const Rational,
    terms: std.ArrayList(Term),

    pub fn init(allocator: Allocator, modulus: []const Rational) Polynomial {
        return .{
            .allocator = allocator,
            .modulus = modulus,
            .terms = std.ArrayList(Term).init(allocator),
        };
    }

    pub fn isZero(self: *const Polynomial) bool {
        return self.terms.items.len == 0;
    }

    pub fn addTerm(self: *Polynomial, index: Index, value: Element) !void {
        if (elementIsZero(value)) return;
        var i: usize = 0;
        while (i < self.terms.items.len) : (i += 1) {
            const term = self.terms.items[i];
            if (term.index.x == index.x and term.index.y == index.y) {
                const next = try elementAdd(self.allocator, self.modulus, term.value, value);
                if (elementIsZero(next)) {
                    _ = self.terms.orderedRemove(i);
                } else {
                    self.terms.items[i].value = next;
                }
                return;
            }
        }
        try self.terms.append(.{ .index = index, .value = value });
        var position = self.terms.items.len - 1;
        while (position > 0 and termLessThan({}, self.terms.items[position], self.terms.items[position - 1])) : (position -= 1) {
            std.mem.swap(Term, &self.terms.items[position], &self.terms.items[position - 1]);
        }
    }

    pub fn add(self: *const Polynomial, other: *const Polynomial) !Polynomial {
        var result = Polynomial.init(self.allocator, self.modulus);
        errdefer result.terms.deinit();
        for (self.terms.items) |term| try result.addTerm(term.index, term.value);
        for (other.terms.items) |term| try result.addTerm(term.index, term.value);
        return result;
    }

    pub fn sub(self: *const Polynomial, other: *const Polynomial) !Polynomial {
        var result = Polynomial.init(self.allocator, self.modulus);
        errdefer result.terms.deinit();
        for (self.terms.items) |term| try result.addTerm(term.index, term.value);
        for (other.terms.items) |term| {
            try result.addTerm(term.index, try elementNeg(self.allocator, term.value));
        }
        return result;
    }

    pub fn mul(self: *const Polynomial, other: *const Polynomial) !Polynomial {
        var result = Polynomial.init(self.allocator, self.modulus);
        errdefer result.terms.deinit();
        for (self.terms.items) |left| {
            for (other.terms.items) |right| {
                const index = Index{
                    .x = try std.math.add(usize, left.index.x, right.index.x),
                    .y = try std.math.add(usize, left.index.y, right.index.y),
                };
                try result.addTerm(index, try elementMul(self.allocator, self.modulus, left.value, right.value));
            }
        }
        return result;
    }

    pub fn derivativeX(self: *const Polynomial) !Polynomial {
        var result = Polynomial.init(self.allocator, self.modulus);
        errdefer result.terms.deinit();
        for (self.terms.items) |term| {
            if (term.index.x == 0) continue;
            try result.addTerm(
                .{ .x = term.index.x - 1, .y = term.index.y },
                try elementScaleInteger(self.allocator, term.value, term.index.x),
            );
        }
        return result;
    }

    pub fn derivativeY(self: *const Polynomial) !Polynomial {
        var result = Polynomial.init(self.allocator, self.modulus);
        errdefer result.terms.deinit();
        for (self.terms.items) |term| {
            if (term.index.y == 0) continue;
            try result.addTerm(
                .{ .x = term.index.x, .y = term.index.y - 1 },
                try elementScaleInteger(self.allocator, term.value, term.index.y),
            );
        }
        return result;
    }

    pub fn evaluate(self: *const Polynomial, x: Element, y: Element) !Element {
        var result = try elementZero(self.allocator, self.modulus.len);
        for (self.terms.items) |term| {
            const xp = try elementPower(self.allocator, self.modulus, x, term.index.x);
            const yp = try elementPower(self.allocator, self.modulus, y, term.index.y);
            const monomial = try elementMul(self.allocator, self.modulus, xp, yp);
            const product = try elementMul(self.allocator, self.modulus, term.value, monomial);
            result = try elementAdd(self.allocator, self.modulus, result, product);
        }
        return result;
    }
};

pub fn rationalZero(allocator: Allocator) !Rational {
    var value = try Rational.init(allocator);
    try value.setInt(0);
    return value;
}

pub fn rationalOne(allocator: Allocator) !Rational {
    var value = try Rational.init(allocator);
    try value.setInt(1);
    return value;
}

pub fn rationalFromInt(allocator: Allocator, value: anytype) !Rational {
    var result = try Rational.init(allocator);
    try result.setInt(value);
    return result;
}

pub fn rationalClone(allocator: Allocator, value: Rational) !Rational {
    var result = try Rational.init(allocator);
    try result.copyRatio(value.p, value.q);
    return result;
}

pub fn rationalAdd(allocator: Allocator, a: Rational, b: Rational) !Rational {
    var result = try Rational.init(allocator);
    try Rational.add(&result, a, b);
    return result;
}

pub fn rationalSub(allocator: Allocator, a: Rational, b: Rational) !Rational {
    var result = try Rational.init(allocator);
    try Rational.sub(&result, a, b);
    return result;
}

pub fn rationalMul(allocator: Allocator, a: Rational, b: Rational) !Rational {
    var result = try Rational.init(allocator);
    try Rational.mul(&result, a, b);
    return result;
}

pub fn rationalDiv(allocator: Allocator, a: Rational, b: Rational) !Rational {
    if (rationalIsZero(b)) return error.DivisionByZero;
    var result = try Rational.init(allocator);
    try Rational.div(&result, a, b);
    return result;
}

pub fn rationalNeg(allocator: Allocator, a: Rational) !Rational {
    var result = try rationalClone(allocator, a);
    result.negate();
    return result;
}

pub fn rationalScaleInteger(allocator: Allocator, value: Rational, factor: anytype) !Rational {
    return rationalMul(allocator, value, try rationalFromInt(allocator, factor));
}

pub fn rationalEqual(a: Rational, b: Rational) bool {
    return a.p.eql(b.p) and a.q.eql(b.q);
}

pub fn rationalCompare(a: Rational, b: Rational) !std.math.Order {
    return Rational.order(a, b);
}

pub fn rationalIsZero(value: Rational) bool {
    return value.p.eqlZero();
}

pub fn rationalText(allocator: Allocator, value: Rational) ![]u8 {
    const numerator = try value.p.toString(allocator, 10, .lower);
    defer allocator.free(numerator);
    var one = try BigInt.initSet(allocator, 1);
    defer one.deinit();
    if (value.q.eqlAbs(one)) return allocator.dupe(u8, numerator);
    const denominator = try value.q.toString(allocator, 10, .lower);
    defer allocator.free(denominator);
    return std.fmt.allocPrint(allocator, "{s}/{s}", .{ numerator, denominator });
}

pub fn elementZero(allocator: Allocator, degree: usize) !Element {
    const values = try allocator.alloc(Rational, degree);
    for (values) |*value| value.* = try rationalZero(allocator);
    return .{ .values = values };
}

pub fn elementOne(allocator: Allocator, degree: usize) !Element {
    const result = try elementZero(allocator, degree);
    if (degree > 0) result.values[0] = try rationalOne(allocator);
    return result;
}

pub fn elementFromRational(allocator: Allocator, degree: usize, value: Rational) !Element {
    const result = try elementZero(allocator, degree);
    if (degree > 0) result.values[0] = try rationalClone(allocator, value);
    return result;
}

pub fn elementIsZero(value: Element) bool {
    for (value.values) |component| {
        if (!rationalIsZero(component)) return false;
    }
    return true;
}

pub fn elementEqual(a: Element, b: Element) bool {
    if (a.values.len != b.values.len) return false;
    var i: usize = 0;
    while (i < a.values.len) : (i += 1) {
        if (!rationalEqual(a.values[i], b.values[i])) return false;
    }
    return true;
}

pub fn elementAdd(allocator: Allocator, modulus: []const Rational, a: Element, b: Element) !Element {
    const degree = modulus.len;
    const result = try allocator.alloc(Rational, degree);
    var i: usize = 0;
    while (i < result.len) : (i += 1) {
        result[i] = try rationalAdd(allocator, a.values[i], b.values[i]);
    }
    return .{ .values = result };
}

pub fn elementNeg(allocator: Allocator, a: Element) !Element {
    const result = try allocator.alloc(Rational, a.values.len);
    var i: usize = 0;
    while (i < result.len) : (i += 1) {
        result[i] = try rationalNeg(allocator, a.values[i]);
    }
    return .{ .values = result };
}

pub fn elementSub(allocator: Allocator, modulus: []const Rational, a: Element, b: Element) !Element {
    return elementAdd(allocator, modulus, a, try elementNeg(allocator, b));
}

pub fn elementScaleRational(allocator: Allocator, value: Element, factor: Rational) !Element {
    const result = try allocator.alloc(Rational, value.values.len);
    var i: usize = 0;
    while (i < result.len) : (i += 1) {
        result[i] = try rationalMul(allocator, value.values[i], factor);
    }
    return .{ .values = result };
}

pub fn elementMul(allocator: Allocator, modulus: []const Rational, a: Element, b: Element) !Element {
    const degree = modulus.len;
    if (degree == 0) return error.InvalidModulus;
    const convolution_len = try std.math.sub(usize, try std.math.mul(usize, degree, 2), 1);
    const convolution = try allocator.alloc(Rational, convolution_len);
    for (convolution) |*value| value.* = try rationalZero(allocator);
    var i: usize = 0;
    while (i < a.values.len) : (i += 1) {
        const left = a.values[i];
        var j: usize = 0;
        while (j < b.values.len) : (j += 1) {
            const right = b.values[j];
            const product = try rationalMul(allocator, left, right);
            convolution[i + j] = try rationalAdd(allocator, convolution[i + j], product);
        }
    }
    var power = convolution_len;
    while (power > degree) {
        power -= 1;
        const coefficient = convolution[power];
        if (rationalIsZero(coefficient)) continue;
        var k: usize = 0;
        while (k < modulus.len) : (k += 1) {
            const modulus_coefficient = modulus[k];
            const product = try rationalMul(allocator, coefficient, modulus_coefficient);
            convolution[power - degree + k] = try rationalSub(allocator, convolution[power - degree + k], product);
        }
    }
    const values = try allocator.alloc(Rational, degree);
    var m: usize = 0;
    while (m < values.len) : (m += 1) {
        values[m] = convolution[m];
    }
    return .{ .values = values };
}

pub fn elementScaleInteger(allocator: Allocator, value: Element, factor: usize) !Element {
    const scalar = try rationalFromInt(allocator, factor);
    const result = try allocator.alloc(Rational, value.values.len);
    var i: usize = 0;
    while (i < result.len) : (i += 1) {
        result[i] = try rationalMul(allocator, value.values[i], scalar);
    }
    return .{ .values = result };
}

pub fn elementPower(allocator: Allocator, modulus: []const Rational, value: Element, exponent: usize) !Element {
    var result = try elementOne(allocator, modulus.len);
    var base = value;
    var remaining = exponent;
    while (remaining != 0) : (remaining >>= 1) {
        if (remaining & 1 != 0) result = try elementMul(allocator, modulus, result, base);
        if (remaining > 1) base = try elementMul(allocator, modulus, base, base);
    }
    return result;
}

pub fn elementText(allocator: Allocator, value: Element) ![]u8 {
    var output = std.ArrayList(u8).init(allocator);
    errdefer output.deinit();
    try output.append('[');
    var i: usize = 0;
    while (i < value.values.len) : (i += 1) {
        if (i > 0) try output.append(',');
        const text = try rationalText(allocator, value.values[i]);
        defer allocator.free(text);
        try output.appendSlice(text);
    }
    try output.append(']');
    return output.toOwnedSlice();
}

fn termLessThan(_: void, a: Term, b: Term) bool {
    const total_a = a.index.x + a.index.y;
    const total_b = b.index.x + b.index.y;
    if (total_a != total_b) return total_a < total_b;
    return a.index.x < b.index.x;
}

pub const Candidate = struct {
    allocator: Allocator,
    modulus: []Rational,
    p: Polynomial,
    q: Polynomial,
    x1: Element,
    y1: Element,
    x2: Element,
    y2: Element,
    u: Element,
    v: Element,

    pub fn build(allocator: Allocator, degree: usize, modulus_degree: usize, codes: []const BigInt) !Candidate {
        if (modulus_degree == 0) return error.InvalidCandidate;
        const d = try BigInt.initSet(allocator, degree);
        var d1 = try BigInt.init(allocator);
        var d2 = try BigInt.init(allocator);
        var monomial_product = try BigInt.init(allocator);
        const two = try BigInt.initSet(allocator, 2);
        var monomial_count = try BigInt.init(allocator);
        var remainder = try BigInt.init(allocator);
        var coefficient_factor = try BigInt.init(allocator);
        var expected_big = try BigInt.init(allocator);
        try BigInt.addScalar(&d1, &d, 1);
        try BigInt.addScalar(&d2, &d, 2);
        try BigInt.mul(&monomial_product, &d1, &d2);
        try BigInt.divFloor(&monomial_count, &remainder, &monomial_product, &two);
        try BigInt.mul(&coefficient_factor, &monomial_count, &two);
        try BigInt.addScalar(&coefficient_factor, &coefficient_factor, 7);
        const big_modulus_degree = try BigInt.initSet(allocator, modulus_degree);
        try BigInt.mul(&expected_big, &big_modulus_degree, &coefficient_factor);
        const expected = expected_big.to(usize) catch return error.ResourceLimit;
        if (codes.len != expected) return error.InvalidCandidate;
        var cursor: usize = 0;
        const modulus = try allocator.alloc(Rational, modulus_degree);
        for (modulus) |*coefficient| {
            coefficient.* = try rationalFromCode(allocator, codes[cursor]);
            cursor += 1;
        }
        var p = Polynomial.init(allocator, modulus);
        var q = Polynomial.init(allocator, modulus);
        var total: usize = 0;
        while (total <= degree) : (total += 1) {
            var x_power: usize = 0;
            while (x_power <= total) : (x_power += 1) {
                const p_coefficient = try readElement(allocator, modulus, codes, &cursor);
                try p.addTerm(.{ .x = x_power, .y = total - x_power }, p_coefficient);
            }
        }
        total = 0;
        while (total <= degree) : (total += 1) {
            var x_power: usize = 0;
            while (x_power <= total) : (x_power += 1) {
                const q_coefficient = try readElement(allocator, modulus, codes, &cursor);
                try q.addTerm(.{ .x = x_power, .y = total - x_power }, q_coefficient);
            }
        }
        const x1 = try readElement(allocator, modulus, codes, &cursor);
        const y1 = try readElement(allocator, modulus, codes, &cursor);
        const x2 = try readElement(allocator, modulus, codes, &cursor);
        const y2 = try readElement(allocator, modulus, codes, &cursor);
        const u = try readElement(allocator, modulus, codes, &cursor);
        const v = try readElement(allocator, modulus, codes, &cursor);
        if (cursor != codes.len) return error.InvalidCandidate;
        return .{
            .allocator = allocator,
            .modulus = modulus,
            .p = p,
            .q = q,
            .x1 = x1,
            .y1 = y1,
            .x2 = x2,
            .y2 = y2,
            .u = u,
            .v = v,
        };
    }

    pub fn accepted(self: *const Candidate) !bool {
        const dx = try elementSub(self.allocator, self.modulus, self.x1, self.x2);
        const dy = try elementSub(self.allocator, self.modulus, self.y1, self.y2);
        const udx = try elementMul(self.allocator, self.modulus, self.u, dx);
        const vdy = try elementMul(self.allocator, self.modulus, self.v, dy);
        const separation_sum = try elementAdd(self.allocator, self.modulus, udx, vdy);
        const one = try elementOne(self.allocator, self.modulus.len);
        const separation = try elementSub(self.allocator, self.modulus, separation_sum, one);
        if (!elementIsZero(separation)) return false;

        const px = try self.p.derivativeX();
        const py = try self.p.derivativeY();
        const qx = try self.q.derivativeX();
        const qy = try self.q.derivativeY();
        const positive = try px.mul(&qy);
        const negative = try py.mul(&qx);
        var jacobian = try positive.sub(&negative);
        try jacobian.addTerm(.{ .x = 0, .y = 0 }, try elementNeg(self.allocator, one));
        if (!jacobian.isZero()) return false;

        const p_at_first = try self.p.evaluate(self.x1, self.y1);
        const p_at_second = try self.p.evaluate(self.x2, self.y2);
        if (!elementIsZero(try elementSub(self.allocator, self.modulus, p_at_first, p_at_second))) return false;

        const q_at_first = try self.q.evaluate(self.x1, self.y1);
        const q_at_second = try self.q.evaluate(self.x2, self.y2);
        if (!elementIsZero(try elementSub(self.allocator, self.modulus, q_at_first, q_at_second))) return false;
        return true;
    }

    pub fn json(
        self: *const Candidate,
        allocator: Allocator,
        stage: []const u8,
        global_index: []const u8,
        local_index: []const u8,
        degree: usize,
        codes: []const BigInt,
    ) ![]u8 {
        var output = std.ArrayList(u8).init(allocator);
        errdefer output.deinit();
        try output.writer().print(
            "{{\"stage\":\"{s}\",\"index\":\"{s}\",\"localIndex\":\"{s}\",\"degree\":{},\"modulusDegree\":{},\"codes\":[",
            .{ stage, global_index, local_index, degree, self.modulus.len },
        );
        var i: usize = 0;
        while (i < codes.len) : (i += 1) {
            if (i != 0) try output.append(',');
            const text = try codes[i].toString(allocator, 10, .lower);
            defer allocator.free(text);
            try appendJsonString(&output, text);
        }
        try output.appendSlice("],\"modulus\":[");
        var k: usize = 0;
        while (k < self.modulus.len) : (k += 1) {
            if (k != 0) try output.append(',');
            const text = try rationalText(allocator, self.modulus[k]);
            defer allocator.free(text);
            try appendJsonString(&output, text);
        }
        try output.appendSlice("],\"rootSelector\":\"the distinct complex root of the monic modulus with lexicographically least (real part, imaginary part)\",\"P\":");
        try appendPolynomialJson(&output, self.p);
        try output.appendSlice(",\"Q\":");
        try appendPolynomialJson(&output, self.q);
        try output.appendSlice(",\"p1\":[");
        try appendElementJson(&output, self.x1);
        try output.append(',');
        try appendElementJson(&output, self.y1);
        try output.appendSlice("],\"p2\":[");
        try appendElementJson(&output, self.x2);
        try output.append(',');
        try appendElementJson(&output, self.y2);
        try output.appendSlice("],\"u\":");
        try appendElementJson(&output, self.u);
        try output.appendSlice(",\"v\":");
        try appendElementJson(&output, self.v);
        try output.appendSlice(",\"checks\":{\"separation\":\"0\",\"jacobianMinusOne\":\"0\",\"Pcollision\":\"0\",\"Qcollision\":\"0\"}}");
        return output.toOwnedSlice();
    }
};

fn readElement(allocator: Allocator, modulus: []const Rational, codes: []const BigInt, cursor: *usize) !Element {
    const value = try allocator.alloc(Rational, modulus.len);
    for (value) |*component| {
        component.* = try rationalFromCode(allocator, codes[cursor.*]);
        cursor.* += 1;
    }
    return .{ .values = value };
}

pub fn rationalFromCode(allocator: Allocator, code: BigInt) !Rational {
    if (code.eqlZero()) return rationalZero(allocator);
    var numerator = try BigInt.init(allocator);
    var denominator = try BigInt.init(allocator);
    if (code.isOdd()) {
        try BigInt.sub(&numerator, &code, &try BigInt.initSet(allocator, 1));
        var quotient = try BigInt.init(allocator);
        var remainder = try BigInt.init(allocator);
        const two = try BigInt.initSet(allocator, 2);
        try BigInt.divFloor(&quotient, &remainder, &numerator, &two);
        const z = quotient;

        var radicand = try BigInt.init(allocator);
        const eight = try BigInt.initSet(allocator, 8);
        try BigInt.mul(&radicand, &z, &eight);
        try BigInt.addScalar(&radicand, &radicand, 1);
        var root = try BigInt.init(allocator);
        try BigInt.sqrt(&root, &radicand);
        var root_minus_one = try BigInt.init(allocator);
        try BigInt.sub(&root_minus_one, &root, &try BigInt.initSet(allocator, 1));
        var w = try BigInt.init(allocator);
        try BigInt.divFloor(&w, &remainder, &root_minus_one, &two);
        var w_plus_one = try BigInt.init(allocator);
        try BigInt.addScalar(&w_plus_one, &w, 1);
        var product = try BigInt.init(allocator);
        try BigInt.mul(&product, &w, &w_plus_one);
        var diagonal = try BigInt.init(allocator);
        try BigInt.divFloor(&diagonal, &remainder, &product, &two);
        var b = try BigInt.init(allocator);
        try BigInt.sub(&b, &z, &diagonal);
        var a = try BigInt.init(allocator);
        try BigInt.sub(&a, &w, &b);
        try BigInt.addScalar(&numerator, &a, 1);
        try BigInt.addScalar(&denominator, &b, 1);
    } else {
        var code_minus_one = try BigInt.init(allocator);
        try BigInt.sub(&code_minus_one, &code, &try BigInt.initSet(allocator, 1));
        var z = try BigInt.init(allocator);
        var remainder = try BigInt.init(allocator);
        const two = try BigInt.initSet(allocator, 2);
        try BigInt.divFloor(&z, &remainder, &code_minus_one, &two);
        var radicand = try BigInt.init(allocator);
        const eight = try BigInt.initSet(allocator, 8);
        try BigInt.mul(&radicand, &z, &eight);
        try BigInt.addScalar(&radicand, &radicand, 1);
        var root = try BigInt.init(allocator);
        try BigInt.sqrt(&root, &radicand);
        var root_minus_one = try BigInt.init(allocator);
        try BigInt.sub(&root_minus_one, &root, &try BigInt.initSet(allocator, 1));
        var w = try BigInt.init(allocator);
        try BigInt.divFloor(&w, &remainder, &root_minus_one, &two);
        var w_plus_one = try BigInt.init(allocator);
        try BigInt.addScalar(&w_plus_one, &w, 1);
        var product = try BigInt.init(allocator);
        try BigInt.mul(&product, &w, &w_plus_one);
        var diagonal = try BigInt.init(allocator);
        try BigInt.divFloor(&diagonal, &remainder, &product, &two);
        var b = try BigInt.init(allocator);
        try BigInt.sub(&b, &z, &diagonal);
        var a = try BigInt.init(allocator);
        try BigInt.sub(&a, &w, &b);
        try BigInt.addScalar(&numerator, &a, 1);
        try BigInt.addScalar(&denominator, &b, 1);
        numerator.negate();
    }
    var result = try Rational.init(allocator);
    try result.copyRatio(numerator, denominator);
    return result;
}

fn appendJsonString(output: *std.ArrayList(u8), text: []const u8) !void {
    try output.append('"');
    for (text) |character| {
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

fn appendElementJson(output: *std.ArrayList(u8), value: Element) !void {
    try output.append('[');
    var i: usize = 0;
    while (i < value.values.len) : (i += 1) {
        if (i != 0) try output.append(',');
        const text = try rationalText(output.allocator, value.values[i]);
        defer output.allocator.free(text);
        try appendJsonString(output, text);
    }
    try output.append(']');
}

fn appendPolynomialJson(output: *std.ArrayList(u8), polynomial: Polynomial) !void {
    try output.append('[');
    var i: usize = 0;
    while (i < polynomial.terms.items.len) : (i += 1) {
        const term = polynomial.terms.items[i];
        if (i != 0) try output.append(',');
        try output.writer().print("{{\"x\":{},\"y\":{},\"coefficient\":", .{ term.index.x, term.index.y });
        try appendElementJson(output, term.value);
        try output.append('}');
    }
    try output.append(']');
}

test "rational code maps every tested code to the prescribed signed fraction" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const samples = [_]struct { code: u64, numerator: i64, denominator: i64 }{
        .{ .code = 0, .numerator = 0, .denominator = 1 },
        .{ .code = 1, .numerator = 1, .denominator = 1 },
        .{ .code = 2, .numerator = -1, .denominator = 1 },
        .{ .code = 3, .numerator = 2, .denominator = 1 },
        .{ .code = 5, .numerator = 1, .denominator = 2 },
        .{ .code = 6, .numerator = -1, .denominator = 2 },
    };
    for (samples) |sample| {
        const code = try BigInt.initSet(allocator, sample.code);
        const actual = try rationalFromCode(allocator, code);
        var expected = try Rational.init(allocator);
        try expected.setRatio(sample.numerator, sample.denominator);
        try std.testing.expect(rationalEqual(actual, expected));
    }
}

test "quotient-ring multiplication reduces every high-degree term" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const modulus = try allocator.alloc(Rational, 2);
    modulus[0] = try rationalOne(allocator);
    modulus[1] = try rationalZero(allocator);
    const t_values = try allocator.alloc(Rational, 2);
    t_values[0] = try rationalZero(allocator);
    t_values[1] = try rationalOne(allocator);
    const t = Element{ .values = t_values };
    const square = try elementMul(allocator, modulus, t, t);
    try std.testing.expectEqual(@as(usize, 2), square.values.len);
    try std.testing.expect(rationalEqual(square.values[0], try rationalFromInt(allocator, -1)));
    try std.testing.expect(rationalIsZero(square.values[1]));
}

test "candidate decoding preserves the exact field order and emits a complete raw certificate" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const codes = try allocator.alloc(BigInt, 9);
    for (codes) |*code| code.* = try BigInt.initSet(allocator, 0);
    const candidate = try Candidate.build(allocator, 0, 1, codes);
    try std.testing.expectEqual(@as(usize, 1), candidate.modulus.len);
    try std.testing.expectEqual(@as(usize, 0), candidate.p.terms.items.len);
    try std.testing.expect(!try candidate.accepted());
    const certificate = try candidate.json(allocator, "0", "0", "0", 0, codes);
    const parsed = try std.json.parseFromSlice(std.json.Value, allocator, certificate, .{});
    try std.testing.expectEqualStrings("0", parsed.value.object.get("stage").?.string);
    try std.testing.expectEqualStrings("0", parsed.value.object.get("index").?.string);
    try std.testing.expectEqual(@as(usize, 9), parsed.value.object.get("codes").?.array.items.len);
    try std.testing.expectEqual(@as(i64, 0), parsed.value.object.get("degree").?.integer);
    try std.testing.expectEqual(@as(i64, 1), parsed.value.object.get("modulusDegree").?.integer);
}

test "rational exact division and rational scaling retain canonical values" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const six = try rationalFromInt(allocator, 6);
    const four = try rationalFromInt(allocator, 4);
    const quotient = try rationalDiv(allocator, six, four);
    const expected = try rationalFromInt(allocator, 3);
    var normalized_half = try Rational.init(allocator);
    try normalized_half.setRatio(3, 2);
    const scaled = try rationalScaleInteger(allocator, quotient, 2);
    try std.testing.expect(rationalEqual(quotient, normalized_half));
    try std.testing.expect(rationalEqual(scaled, expected));
}