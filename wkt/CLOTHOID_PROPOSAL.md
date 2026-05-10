# Proposal: `CLOTHOID` segment in WKT (JTS extension)

> **AI Disclosure** *(per the [Eclipse Foundation Generative AI Usage Guidelines for Committers](https://www.eclipse.org/projects/guidelines/genai/), applied as best-practice hygiene even though grammars-v4 is not an Eclipse project)*
>
> This proposal was largely AI-drafted and human-reviewed/curated. The technical decisions in §3 are author-owned. The AI-generated portions are dedicated to the public domain (CC0-1.0); human curation is subject to the host repository's licence.
>
> ```
> SPDX-License-Identifier: BSD-3-Clause AND CC0-1.0
> Assisted-by: xAI Grok (grok-4.3)
> Assisted-by: Claude (Opus-4.7)
> ```

**Status:** Draft proposal + minimal grammar landed (lexer token, `clothoidGeometry` parser rule, wired into `compoundCurveMember`). Upstream PR open at [antlr/grammars-v4#4848](https://github.com/antlr/grammars-v4/pull/4848). Reference Java implementation (geometry types, `CurvedWKTReader`/`Writer`, renderer, TestBuilder UI for editing/inserting clothoids) lives at <https://github.com/grootstebozewolf/jts/tree/feature/sfa-curve-clothoid-playground>.
**Audience:** the JTS / NetTopologySuite curve-geometry community, and (later) `antlr/grammars-v4` reviewers.
**Companion:** sister proposal under [locationtech/jts#1195 — SFA Curve Awareness epic](https://github.com/locationtech/jts/issues/1195).

## 1. Summary

Extend WKT — *as a JTS extension, beyond OGC SFA / ISO 19125-2* — with a `CLOTHOID` segment usable inside `COMPOUNDCURVE`. A clothoid (Euler / Cornu spiral) is the canonical transition curve in railway and highway alignment design: it interpolates curvature linearly with arc length, smoothing the lateral-jerk discontinuity that occurs at a bare line→arc seam.

**Restricted scope.** `CLOTHOID` is valid *only* as a non-leading segment inside `COMPOUNDCURVE`. It is not a top-level WKT geometry, and not allowed elsewhere. This restriction is what makes the syntax unambiguous, the parser simple, and the implementation tractable.

## 2. Why this is a JTS extension, not an OGC change

OGC SFA and ISO 19125-2 define `LINESTRING`, `CIRCULARSTRING`, `COMPOUNDCURVE`, `CURVEPOLYGON`, `MULTICURVE`, `MULTISURFACE`. **None of them define a clothoid.** Civil-engineering domain models do — IFC 4.3 has `IfcClothoid`, LandXML has `<Spiral spiType="clothoid">` — but the GIS/SFA family deliberately stops at circular arcs.

This proposal therefore lives in the same space as `CIRCULARSTRING` itself does for OGC SFA: an *extension* over a base spec, not a modification of it. Existing OGC-conformant WKT readers are expected to fail on the unknown `CLOTHOID` keyword (correct OGC behaviour for an unknown extension). JTS readers in extension mode accept it.

If/when ISO publishes a clothoid keyword in a future spec revision, this proposal becomes the migration path; until then, the `CLOTHOID` token and grammar live behind a `JTS_WKT_CLOTHOID_EXTENSION` opt-in.

## 3. Locked decisions

The v0 sketch left ten engineering decisions open. This section pins each one down. Future review may revisit, but implementations must follow these unless a decision is explicitly overturned.

### 3.1 Sign convention for curvature

**Positive κ = counter-clockwise turn** in the standard mathematics XY-up coordinate system (positive y = up, positive x = right). Equivalently: a positive-κ clothoid bends the tangent vector toward the +y half-plane.

For renderers using screen Y-down coordinates, this inverts at the rendering layer; the geometry layer is XY-up by convention.

### 3.2 First-segment clothoid

**Disallowed.** A `CLOTHOID(k0, k1, L)` may not be the first segment of a `COMPOUNDCURVE`. The clothoid inherits its start point, start tangent direction and start curvature from the immediately preceding segment. With no preceding segment, those values are undefined.

If a future use case needs a free-standing or leading clothoid, the syntax can be extended additively (e.g. a 6-arg form `CLOTHOID(x0 y0, theta0, k0, k1, L)` allowed only in first position), without breaking the restricted form. Not in v1.

### 3.3 Junction tolerance — who wins on coord disagreement

**Typed coordinate wins.** The next segment's explicitly typed start coordinate is authoritative. The clothoid's analytically computed end coordinate is *informational*. The parser computes the analytical end; if drift from the typed coord exceeds `1e-9` (relative to chord length), it emits a parser **warning** (configurable to fail in `strict` mode), but the typed coord is what the constructed geometry uses.

Rationale: input WKT typically rounds to 6–9 significant figures; the analytical end of a Fresnel integral is exact to ~15 figures. Forcing the analytical value would silently change user-typed coords, which is worse than tolerating documented drift.

### 3.4 G2 continuity at clothoid → arc junction

**Warn-only at parse time.** When a `CLOTHOID(k0, k1, L)` segment is followed by a `CIRCULARSTRING(...)`, the parser computes the circumscribed-circle radius `R` of the arc's three control points and checks whether `|1/R − |k1|| < 1e-6` (relative). Mismatch emits a warning. `strict` mode upgrades to fail.

A clothoid → line junction implies `k1 = 0`; the same check applies (warn if `|k1| > 1e-6`).

### 3.5 Numeric backend for integration

**Adaptive composite Simpson's rule** on the heading function

```
θ(s) = θ₀ + κ₀·s + ½·(κ₁ − κ₀)/L · s²
```

producing position via

```
x(s) = x₀ + ∫₀ˢ cos(θ(t)) dt
y(s) = y₀ + ∫₀ˢ sin(θ(t)) dt
```

Convergence target: `1e-10` relative on each ordinate over `[0, L]`, capped at 14 levels of refinement. No external dependency. Works directly in the geometry's native (x₀, θ₀, κ₀, κ₁, L) parametrisation without translating to canonical Fresnel form.

A power-series Fresnel approximation (Heald 1985, ~5 terms, 1e-10 accuracy on [-π/2, π/2]) is acceptable as an alternative for the canonical form, but adaptive Simpson's covers the general case directly and is preferred.

### 3.6 `getCoordinateSequence()` semantics on a clothoid-bearing CompoundCurve

A `CompoundCurve` whose member list contains a `ClothoidSegment` returns, when flattened, **just the start and end coordinates of that segment**. The interior of the clothoid is not represented in the flat coord sequence.

Algorithms that walk `getCoordinates()` on such a geometry under-represent its actual extent. The remedy is to call `toLinear(tolerance)` first; this is documented as a contract of the type. Library users that mix clothoid-bearing curves with algorithms that don't know about `toLinear` should densify explicitly.

### 3.7 `equalsExact` semantics

`ClothoidSegment.equalsExact(other)` returns `true` iff:

1. `other` is also a `ClothoidSegment`, AND
2. `(k0, k1, L)` are bitwise equal (within ordinary `equalsExact` tolerance), AND
3. The start state — `(startPoint, startTangent, startKappa)` — is also equal.

Two clothoids with identical `(k0, k1, L)` but different start state are different geometries. Inside a `CompoundCurve`, the start state is implicit; equality of the containing `CompoundCurve`s implies equality of all segment start states.

### 3.8 `reverse()`

`CLOTHOID(k0, k1, L)` reverses to `CLOTHOID(−k1, −k0, L)` — the parameter order swaps *and* the signs flip, because reversing the traversal direction inverts the sense of the tangent rotation, hence inverts the sign of curvature. The reversed segment starts from the original end point with the original end tangent, rotated 180°.

Implication: `Geometry.reverse()` on a `CompoundCurve(line, clothoid, arc)` yields `CompoundCurve(arc.reverse(), clothoid.reverseSigns(), line.reverse())` — straightforward member-list reversal with each member's own reverse semantics.

### 3.9 Bounding box

Computed **analytically** by solving `dx/ds = cos(θ(s)) = 0` and `dy/ds = sin(θ(s)) = 0` for `s ∈ [0, L]`. These reduce to roots of `θ(s) = (n + ½)π` and `θ(s) = nπ` respectively. With `θ(s) = θ₀ + κ₀·s + ½·(κ₁−κ₀)/L · s²` quadratic in `s`, each equation is a (possibly-empty) finite set of quadratic roots clipped to `[0, L]`.

The clothoid's envelope is the bounding box of `{startPoint, endPoint, x(s_root), y(s_root) for each root}`.

If the analytical solver fails to converge or produces NaN (e.g. for `κ₁ = κ₀`, the segment degenerates and the linear / circular bounding-box code applies), fall back to a densified bounding box plus a 1% margin.

### 3.10 Canonical parameter form

WKT carries `(k0, k1, L)`. The LandXML/IFC `A` constant is derived:

```
A = √(L / |κ₁ − κ₀|)        (general)
A = √(L / κ₁)                 (when κ₀ = 0, the typical entry-spiral case)
```

The Java type provides `getClothoidConstantA()` returning `A`, and a static factory

```java
ClothoidSegment.fromAandLength(double A, double startKappa, double length)
```

so users coming from LandXML/IFC don't have to derive the conversion themselves.

`κ₁ = κ₀` is rejected at construction (it's a circular arc or straight line, not a clothoid; users should use `CIRCULARSTRING` or a bare LineString segment instead).

## 4. Syntax

```
CLOTHOID ( startKappa , endKappa , length )
```

- Three numeric scalars, comma-separated, inside parens.
- Each scalar is the same numeric literal grammar as a coordinate ordinate (signed decimal, optional exponent, with `INF`/`-INF`/`NAN` permitted by the surrounding extension grammar).
- `length > 0`. Negative length is rejected.
- `startKappa = endKappa` is rejected (degenerate; use `CIRCULARSTRING` or a line segment).

The `CLOTHOID` token is **only** valid as a `compoundCurveMember` non-leading position. Other positions error out at parse time.

### 4.1 Worked example (highway entry spiral)

```wkt
COMPOUNDCURVE (
  (0 0, 100 0),
  CLOTHOID (0, 0.005, 80),
  CIRCULARSTRING (180.0 0.05333, 196.7 14.4, 195.0 32.5),
  CLOTHOID (0.005, 0, 80),
  (231 75, 300 75)
)
```

Reads as: a 100-unit straight, an 80-unit entry clothoid taking curvature from 0 to 0.005 (R = 200 inside the bend), a circular arc of radius 200, an 80-unit exit clothoid back to straight, and a final straight. The two `CIRCULARSTRING` and the trailing line-segment start coordinates must agree (within tolerance) with the analytical end of the preceding clothoid.

### 4.2 What the grammar already enforces

- Lexer fails on the `CLOTHOID` token if the parser is in OGC-strict mode (extension off).
- Parser fails fast if `CLOTHOID` appears as the first member of a `COMPOUNDCURVE`, or at the top level (matching §3.2).
- `length` and `kappa` parse via the existing numeric ordinate rule, so `INF`/`NAN` are syntactically accepted but semantically rejected at construction (length must be positive finite; kappa must be finite).

## 5. Grammar diff (landed in this branch)

Against the post-#4846 `wkt/wkt.g4` (which has `compoundCurveGeometry` and a `compoundCurveMember` production):

```antlr
// Lexer addition
CLOTHOID : C L O T H O I D ;

// Parser addition (rule name follows the existing "<keyword>Geometry" convention)
clothoidGeometry
    : CLOTHOID LPAR ordinate COMMA ordinate COMMA ordinate RPAR
    ;

// Parser change: extend compoundCurveMember
compoundCurveMember
    : lineStringText
    | circularStringGeometry
    | clothoidGeometry             // ← new
    ;
```

A separate semantic-validation pass (not the grammar) enforces:
- `clothoidGeometry` cannot be the first member of a `compoundCurveGeometry`,
- `length` ordinate is positive finite,
- `startKappa ≠ endKappa`.

These belong outside the grammar so error messages can carry meaningful context, and so the pure ANTLR grammar stays decision-free.

## 6. Compatibility & fallback

Existing OGC WKT readers will fail on the `CLOTHOID` keyword. This is the *correct* OGC behaviour for an unknown extension keyword. The proposal does not advocate silent fallback or chord-replacement.

For interop with non-extension consumers, a `WKTWriter.writeWithFallback(Geometry, FallbackPolicy)` mode is suggested:

- `FAIL` (default): emit the geometry as-is including `CLOTHOID`; non-extension readers will fail.
- `DENSIFY`: replace each `CLOTHOID` segment with its `toLinear(tolerance)` chord polyline before emitting WKT. Result is OGC-valid `LINESTRING` / `COMPOUNDCURVE` of `LINESTRING`+`CIRCULARSTRING`.
- `CHORD`: replace each `CLOTHOID` with a single straight line from start to end. Loses the transition.

This is implementation guidance; not part of the grammar proposal itself.

## 7. Implementation status

The reference Java implementation is **complete** on the JTS playground branch
[`feature/sfa-curve-clothoid-playground`](https://github.com/grootstebozewolf/jts/tree/feature/sfa-curve-clothoid-playground).
The structure mirrors how `CircularString` / `CompoundCurve` are laid out:

- **`ClothoidSegment`** (geometry) — extends `LineString`, stores `(startPoint, startTangent, startKappa, endKappa, length)`. Implements `Linearizable.toLinear(tolerance)` via §3.5.
- **`CompoundCurve` with structural members** — accepts `ClothoidSegment` alongside `LineString` / `CircularString` members; flat coord sequence is the dedup-concatenation of member coords (§3.6).
- **`CurvedWKTReader.readClothoidSegmentText`** — reads the three scalars and constructs a `ClothoidSegment` using the running state (point + tangent + curvature) from preceding `CompoundCurve` members; emits the junction-drift warning (§3.3) when the next typed coordinate disagrees with the analytical end.
- **`CurvedWKTWriter.appendClothoidSegmentText`** — emits the three scalars; junction coordinates come from surrounding members.
- **`CurvedShapeWriter`** — `CompoundCurve` walker dispatches each member to a type-specific renderer: `ClothoidSegment` via `toLinear(0.5)` chord-stream, `CircularString` via cubic-Bezier arc approximation, plain `LineString` straight-through.
- **TestBuilder integration** — `ClothoidPanel` provides parameter editing of any selected `ClothoidSegment` with a cascade rigid-frame transform on Apply (so downstream members translate + rotate to maintain G2 continuity), an "Insert spiral before next arc" action that fits a spiral easement at any tangent-continuous `LineString → CircularString` junction, and an inspect button that displays κ / θ / L / R for clothoid and arc members.

Total surface: ~1500 LoC including the editing UI. The grammar half — token + rule + example — landed in [antlr/grammars-v4#4848](https://github.com/antlr/grammars-v4/pull/4848).

### 7.1 Operational characteristics (v1)

These are the things reviewers and downstream users will observe; documented here so the proposal stays the canonical reference and the PR thread doesn't have to be the source of truth.

**Buffering and other spatial ops go through full linearization.** `BufferOp` (and `intersection`, `union`, etc.) call `((Linearizable) g).toLinear(tol)` and operate on the resulting chord polyline. This is intentional for v1: the parallel of a clothoid is *not* itself a clothoid (no closed-form arc-length parameterisation), and the parallel of a circular arc remains a circular arc only if the caller accepts a different radius (and the construction degenerates for inward offsets when distance ≥ R). For v1 we retain the densify-then-buffer approach because it is predictable, the geometric error is bounded by the caller-supplied tolerance, and it avoids the substantially larger problem of a true curve-aware offset algorithm (which belongs in a separate proposal).

**Artifacts to watch in offset / buffer output.**

- *Tangent kinks at user-authored junctions* render as the configured join style (Round / Mitre / Bevel) instead of smoothing across — a feature, not a bug, but worth knowing if a buffer comes back with corners. The TestBuilder edit operations (Apply, Insert spiral) preserve G1 + G2 continuity, but hand-authored WKT can absolutely create kinks.
- *Very tight clothoids* — the densifier's chord-to-true-curve drift is bounded by `tol`, but the buffer polygon's vertices inherit that drift. Visible as minor faceting / short straight segments on the offset polygon in regions of very high curvature. Tighter `tol` makes it disappear at the cost of more vertices.
- *Cap orientation at clothoid endpoints* uses the analytical end tangent, not chord direction. A naive "last two coords" implementation would be subtly wrong here — the clothoid's flat coord sequence is just `(start, end)` (per §3.6), so chord direction equals the *secant*, not the tangent.

**Cost vs a plain `LineString` of equivalent densified-point count.** Equivalent within noise. `CompoundCurve` adds a structural member array (a few hundred bytes for typical chains of 5–10 members) but the flat coord sequence concatenates member coords, so spatial ops see the same point count regardless of structural layering. Repeated `toLinear(tol)` calls are not memoized in v1 — that's a future optimisation if a profiler shows it matters; in practice spatial ops dominate.

## 8. Relationship to the SFA Curve Awareness epic

The SFA Curve Awareness epic ([locationtech/jts#1195](https://github.com/locationtech/jts/issues/1195)) is about preserving *spec-defined* curve types (`CIRCULARSTRING`, `COMPOUNDCURVE`, `CURVEPOLYGON`, etc.) through every JTS algorithm. **This proposal is explicitly out of scope for that epic** (and was carved out as a footnote in §3 of that epic for exactly this reason).

`CLOTHOID` is a separate, sibling effort:
- Different precedent (civil engineering, not GIS).
- Different spec (none yet — JTS would lead).
- Different consumers (rail/road/CAD tooling).

The two efforts share infrastructure (`CompoundCurve` member-list architecture, `Linearizable` interface, `CurvedShapeWriter` rendering pipeline), so the SFA work *unblocks* this; but the SFA epic should not block on this. Land SFA first.

## 9. Open questions / future work

- **Multi-segment clothoid splines.** Real-world transitions sometimes use back-to-back clothoid segments to achieve C³ continuity (jerk-smooth). The grammar permits this — multiple `CLOTHOID(...)` in a row is well-defined — but no special syntax is needed. Document as a usage pattern in v1.
- **Top-level / leading clothoid.** Deferred to v2 if needed (see §3.2). Likely 6-arg `CLOTHOID(x0 y0, theta0, k0, k1, L)` form in first position only.
- **`ClothoidString`** (analogue of `CircularString` — a chain of clothoid arcs sharing endpoints). Possible v3; covers the rare case of a continuous clothoid sequence not interrupted by circles or straights.
- **Z / M ordinates.** The proposal treats clothoid as 2-D. Z/M handling for a clothoid is the same Z/M handling we have for arcs today — propagate from start/end, do not interpolate. Document.
- **Analytical clothoid–clothoid intersection.** Non-elementary (involves intersections of Fresnel-defined curves). Deferred to *much* later; for v1, intersections are computed via `toLinear` densification and the polyline machinery.

## 10. References

- OGC Simple Feature Access 1.2.1 / ISO 19125-2 — for what WKT does and doesn't define.
- IFC 4.3 — `IfcClothoid` type and parametrisation.
- LandXML 1.2 — `<Spiral spiType="clothoid" ...>` element.
- Heald, M. A. (1985). "Rational approximations for the Fresnel integrals." *Mathematics of Computation*, 44(170), 459–461.
- Cornu, M. A. (1874). "Méthode nouvelle pour la discussion des problèmes de diffraction." (origin of "Cornu spiral".)
- IEEE Trans. Intelligent Transportation Systems — multiple papers on clothoid path planning for autonomous vehicles, useful for densification-error budgets.
