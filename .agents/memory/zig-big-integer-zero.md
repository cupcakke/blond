---
name: Zig big-integer zero semantics
description: Signed-zero behavior to account for when comparing Zig 0.11 managed big integers.
---

In Zig 0.11, `std.math.big.int.Managed.order` can report an ordering between two intermediate values that both satisfy `eqlZero()`. Treat two zero values as equal before relying on ordering or exact-equality checks.

**Why:** Subtracting equal values during weak-composition enumeration produced zero values whose `order` result incorrectly rejected a valid zero-sum candidate. Allocation-failure tests exposed the same state path.

**How to apply:** For nonnegative checks, accept `eqlZero()` explicitly before checking the sign. For equality, check both `eqlZero()` results before using `eql`.