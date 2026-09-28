from __future__ import annotations
import sympy as sp

x, y, t = sp.symbols("x y t")
U, V = sp.symbols("U V")


def jacobian_minus_one(P, Q):
    return sp.expand(sp.diff(P, x)*sp.diff(Q, y) - sp.diff(P, y)*sp.diff(Q, x) - 1)


def xy_coeff_equations(expr):
    expr = sp.expand(expr)
    if expr == 0:
        return []
    poly = sp.Poly(expr, x, y)
    return [sp.expand(c) for _, c in poly.terms()]


def jacobian_equations(P, Q):
    return xy_coeff_equations(jacobian_minus_one(P, Q))


def normalized_dense(DP, DQ):
    P = x
    Q = y
    variables = []

    for total in range(2, DP + 1):
        for i in range(total, -1, -1):
            j = total - i
            a = sp.symbols(f"a_{i}_{j}")
            variables.append(a)
            P += a * x**i * y**j

    for total in range(2, DQ + 1):
        for i in range(total, -1, -1):
            j = total - i
            b = sp.symbols(f"b_{i}_{j}")
            variables.append(b)
            Q += b * x**i * y**j

    return sp.expand(P), sp.expand(Q), variables


def normalized_collision_equations(P, Q):
    return [
        sp.expand(P.subs({x: 1, y: 0})),
        sp.expand(Q.subs({x: 1, y: 0})),
    ]


def verify_candidate(P, Q, p1=(0, 0), p2=(1, 0)):
    p1x, p1y = map(sp.sympify, p1)
    p2x, p2y = map(sp.sympify, p2)

    Jm = sp.factor(jacobian_minus_one(P, Q))
    dP = sp.factor(P.subs({x: p1x, y: p1y}) - P.subs({x: p2x, y: p2y}))
    dQ = sp.factor(Q.subs({x: p1x, y: p1y}) - Q.subs({x: p2x, y: p2y}))

    print("J_minus_1 =", Jm)
    print("Delta_P  =", dP)
    print("Delta_Q  =", dQ)

    return sp.expand(Jm) == 0 and sp.expand(dP) == 0 and sp.expand(dQ) == 0


def fiber_resultant_certificate(P, Q):
    R = sp.factor(sp.resultant(P - U, Q - V, y))
    print("Resultant_y(P-U,Q-V) =")
    print(R)
    print("generic degree in x =", sp.Poly(R, x).degree())
    return R


def dense_groebner_search(DP, DQ, order="grevlex"):
    P, Q, vars_ = normalized_dense(DP, DQ)
    eqs = jacobian_equations(P, Q) + normalized_collision_equations(P, Q)
    G = sp.groebner(eqs, *vars_, order=order)
    print("P =", P)
    print("Q =", Q)
    print("number of equations =", len(eqs))
    print("number of variables  =", len(vars_))
    print("Groebner basis =", [g.as_expr() for g in G.polys])
    return P, Q, eqs, G


def laurent_coefficients(expr):
    expr = sp.expand(expr)
    out = {}
    for term in sp.Add.make_args(expr):
        exp = sp.sympify(term.as_powers_dict().get(t, 0))
        coeff = sp.simplify(term / (t**exp))
        out[exp] = sp.expand(out.get(exp, 0) + coeff)
    return dict(sorted(out.items(), key=lambda kv: kv[0]))


def finite_branch_equations(P, Q, xser, yser):
    eqs = []
    for R in (P, Q):
        S = sp.expand(R.subs({x: xser, y: yser}))
        for exp, coeff in laurent_coefficients(S).items():
            if exp < 0:
                eqs.append(sp.expand(coeff))
    return eqs


def name_with_exp(prefix, k):
    return f"{prefix}_p{k}" if k >= 0 else f"{prefix}_m{-k}"


def branch_series(m, n, N):
    """
    Laurent branch as t -> 0:
        x(t) = t^(-m) + lower pole/regular terms
        y(t) = v_{-n} t^(-n) + ...
    leading x coefficient fixed to 1 by reparametrization.
    """
    bvars = []

    xser = t**(-m)
    for k in range(-m + 1, N + 1):
        u = sp.symbols(name_with_exp("u", k))
        bvars.append(u)
        xser += u * t**k

    yser = 0
    lead_y = None
    for k in range(-n, N + 1):
        v = sp.symbols(name_with_exp("v", k))
        bvars.append(v)
        yser += v * t**k
        if k == -n:
            lead_y = v

    rho = sp.symbols("rho")
    bvars.append(rho)
    nonzero_lead_eq = rho * lead_y - 1

    return sp.expand(xser), sp.expand(yser), bvars, nonzero_lead_eq


def dicritical_system(DP, DQ, m, n, N):
    """
    Infinity-first system:
      1. Keller equations.
      2. normalized collision.
      3. existence of Laurent branch with finite P,Q values.
    """
    P, Q, cvars = normalized_dense(DP, DQ)
    xser, yser, bvars, nonzero_eq = branch_series(m, n, N)

    eqs = []
    eqs += jacobian_equations(P, Q)
    eqs += normalized_collision_equations(P, Q)
    eqs += finite_branch_equations(P, Q, xser, yser)
    eqs += [nonzero_eq]

    vars_ = cvars + bvars
    return P, Q, xser, yser, eqs, vars_


def run_dicritical_groebner(DP, DQ, m, n, N, order="grevlex"):
    P, Q, xser, yser, eqs, vars_ = dicritical_system(DP, DQ, m, n, N)
    print("P degree bound =", DP)
    print("Q degree bound =", DQ)
    print("x(t) =", xser)
    print("y(t) =", yser)
    print("number of equations =", len(eqs))
    print("number of variables  =", len(vars_))

    G = sp.groebner(eqs, *vars_, order=order)
    print("Groebner basis =", [g.as_expr() for g in G.polys])
    return P, Q, xser, yser, eqs, vars_, G


if __name__ == "__main__":
    P, Q, _ = normalized_dense(2, 2)
    print("P =", P)
    print("Q =", Q)