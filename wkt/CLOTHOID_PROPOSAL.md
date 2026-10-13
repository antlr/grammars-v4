# Proposal: `CLOTHOID` segment in WKT (JTS extension)

> **AI Disclosure** *(per the [Eclipse Foundation Generative AI Usage Guidelines for Committers](https://www.eclipse.org/projects/guidelines/genai/), applied as best-practice hygiene even though grammars-v4 is not an Eclipse project)*
>
> This proposal was largely AI-drafted, then reviewed and curated by a human. The technical decisions in §3 are author-owned. The AI-generated portions are dedicated to the public domain (CC0-1.0); human curation is subject to the host repository's licence.
>
> ```
> SPDX-License-Identifier: BSD-3-Clause AND CC0-1.0
> Assisted-by: xAI Grok (grok-4.3)
> Assisted-by: Claude (Opus-4.7)
> ```

**Status:** Grammar landed in [antlr/grammars-v4#4848](https://github.com/antlr/grammars-v4/pull/4848) — lexer token, `clothoidGeometry` parser rule, wired into `compoundCurveMember`. The ISO/IEC 13249-3 clothoid now coexists with it; see §5.1. A reference Java implementation (geometry types, `CurvedWKTReader`/`Writer`, renderer, TestBuilder UI for editing and inserting clothoids) lives at <https://github.com/grootstebozewolf/jts/tree/feature/sfa-curve-clothoid-playground>.

**Audience:** the JTS / NetTopologySuite curve-geometry community, and `antlr/grammars-v4` reviewers.

**Companion:** sister proposal under [locationtech/jts#1195 — SFA Curve Awareness epic](https://github.com/locationtech/jts/issues/1195).

## 1. Summary

Extend WKT — *as a JTS extension, beyond OGC SFA / ISO 19125-2* — with a `CLOTHOID` segment usable inside `COMPOUNDCURVE`. A clothoid (Euler / Cornu spiral) is the canonical transition curve in railway and highway alignment design: it interpolates curvature linearly with arc length, removing the curvature step — and with it the jolt in lateral acceleration — that a bare line→arc seam produces.

**Restricted scope.** `CLOTHOID` is valid *only* as a non-leading segment inside `COMPOUNDCURVE`. It is not a top-level WKT geometry, and not allowed elsewhere. This restriction is what makes the syntax unambiguous, the parser simple, and the implementation tractable.

## 2. Why this is a JTS extension, not an OGC change

OGC SFA 1.2.1 and ISO 19125 define `LINESTRING`, `CIRCULARSTRING`, `COMPOUNDCURVE`, `CURVEPOLYGON`, `MULTICURVE` and `MULTISURFACE`. **None of them define a clothoid.** Civil-engineering domain models do — IFC 4.3 has `IfcClothoid`, LandXML has `<Spiral spiType="clothoid">` — but the GIS/SFA family deliberately stops at circular arcs.

This proposal is therefore an *extension* over a base spec, not a modification of it. Existing OGC-conformant WKT readers are expected to fail on the unknown `CLOTHOID` keyword (correct OGC behaviour for an unknown extension). JTS readers in extension mode accept it.

ISO, as it turns out, already published one: subclause 5.1.67 of ISO/IEC 13249-3 defines a clothoid of its own, on a different parametrisation. §5.1 covers what that form looks like and how the two coexist. The `CLOTHOID` token described here stays behind a `JTS_WKT_CLOTHOID_EXTENSION` opt-in.

## 3. Locked decisions

An earlier draft of this proposal left ten engineering decisions open. This section pins each one down. Future review may revisit them, but implementations must follow these unless a decision is explicitly overturned.

Curvature is written κ₀ (start) and κ₁ (end) throughout, and arc length L.

### 3.1 Sign convention for curvature

**Positive κ = counter-clockwise turn** in the standard mathematical convention, with positive y up and positive x right. Equivalently: a positive-κ clothoid bends the tangent vector toward the +y half-plane.

For renderers using screen Y-down coordinates, this inverts at the rendering layer; the geometry layer is XY-up by convention.

### 3.2 First-segment clothoid

**Disallowed.** A `CLOTHOID(κ₀, κ₁, L)` may not be the first segment of a `COMPOUNDCURVE`. The clothoid inherits its start point, start tangent direction and start curvature from the immediately preceding segment. With no preceding segment, those values are undefined.

If a future use case needs a free-standing or leading clothoid, the syntax can be extended additively — for example a six-scalar form `CLOTHOID(x₀ y₀, θ₀, κ₀, κ₁, L)` allowed only in first position — without breaking the restricted form. Not in v1.

### 3.3 Junction tolerance — who wins when coordinates disagree

**The typed coordinate wins.** The next segment's explicitly typed start coordinate is authoritative. The clothoid's analytically computed end coordinate is *informational*. The parser computes the analytical end; if drift from the typed coordinate exceeds `1e-9` (relative to chord length), it emits a parser **warning** (configurable to fail in `strict` mode), but the typed coordinate is what the constructed geometry uses.

Rationale: input WKT typically rounds to 6–9 significant figures, whereas a double-precision evaluation of the Fresnel integral is good to roughly 15. Forcing the analytical value would silently change user-typed coordinates, which is worse than tolerating documented drift.

### 3.4 G2 continuity at clothoid → arc junction

**Warn-only at parse time.** When a `CLOTHOID(κ₀, κ₁, L)` segment is followed by a `CIRCULARSTRING(...)`, the parser computes the circumscribed-circle radius `R` of the arc's three control points and checks whether `| 1/R − |κ₁| | < 1e-6` (relative). Mismatch emits a warning. `strict` mode upgrades to fail.

A clothoid → line junction implies `κ₁ = 0`; the same check applies, warning if `|κ₁| > 1e-6`.

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

Convergence target: `1e-10` relative on each ordinate over `[0, L]`, capped at 14 levels of refinement. No external dependency. Works directly in the geometry's native (x₀, y₀, θ₀, κ₀, κ₁, L) parametrisation without translating to canonical Fresnel form.

A power-series Fresnel approximation (Heald 1985, ~5 terms, 1e-10 accuracy on [-π/2, π/2]) is acceptable as an alternative for the canonical form, but adaptive Simpson's rule covers the general case directly and is preferred.

### 3.6 Flattened-coordinate semantics on a clothoid-bearing CompoundCurve

A `CompoundCurve` whose member list contains a `ClothoidSegment` returns, when flattened, **just the start and end coordinates of that segment**. The interior of the clothoid is not represented in the flat coordinate sequence.

Algorithms that walk `getCoordinates()` or `getCoordinateSequence()` on such a geometry therefore under-represent its actual extent. The remedy is to call `toLinear(tolerance)` first; this is documented as a contract of the type. Callers who mix clothoid-bearing curves with algorithms that do not know about `toLinear` should densify explicitly.

### 3.7 `equalsExact` semantics

`ClothoidSegment.equalsExact(other)` returns `true` iff:

1. `other` is also a `ClothoidSegment`, AND
2. κ₀, κ₁ and L compare equal under the ordinary `equalsExact` tolerance, AND
3. the start state — start point, start tangent and start curvature — is also equal.

Two clothoids with identical (κ₀, κ₁, L) but different start state are different geometries. Inside a `CompoundCurve`, the start state is implicit; equality of the containing `CompoundCurve`s implies equality of all segment start states.

### 3.8 `reverse()`

`CLOTHOID(κ₀, κ₁, L)` reverses to `CLOTHOID(−κ₁, −κ₀, L)` — the parameter order swaps *and* the signs flip, because reversing the traversal direction inverts the sense of the tangent rotation, hence inverts the sign of curvature. The reversed segment starts from the original end point with the original end tangent, rotated 180°.

Implication: `Geometry.reverse()` on a `CompoundCurve(line, clothoid, arc)` yields `CompoundCurve(arc.reverse(), clothoid.reverse(), line.reverse())` — straightforward member-list reversal, with each member applying its own reverse semantics.

### 3.9 Bounding box

Computed **analytically** by solving `dx/ds = cos(θ(s)) = 0` and `dy/ds = sin(θ(s)) = 0` for `s ∈ [0, L]`. These reduce to roots of `θ(s) = (n + ½)π` and `θ(s) = nπ` respectively. With `θ(s) = θ₀ + κ₀·s + ½·(κ₁−κ₀)/L · s²` quadratic in `s`, each equation has a possibly-empty finite set of roots, clipped to `[0, L]`.

The clothoid's envelope is the bounding box of `{startPoint, endPoint, x(s_root), y(s_root) for each root}`.

If the root solve degenerates numerically and produces NaN, fall back to a densified bounding box plus a 1% margin. The `κ₁ = κ₀` case does not arise here: it is rejected at construction (§3.10).

### 3.10 Canonical parameter form

WKT carries (κ₀, κ₁, L). The LandXML/IFC `A` constant is derived:

```
A = √(L / |κ₁ − κ₀|)        (general)
A = √(L / |κ₁|)                 (when κ₀ = 0, the typical entry-spiral case)
```

The Java type provides `getClothoidConstantA()` returning `A`, and a static factory

```java
ClothoidSegment.fromAandLength(double A, double startKappa, double length)
```

so users coming from LandXML/IFC do not have to derive the conversion themselves.

`κ₁ = κ₀` is rejected at construction: that is a circular arc or a straight line, not a clothoid, and users should reach for `CIRCULARSTRING` or a bare LineString segment instead.

## 4. Syntax

```
CLOTHOID ( startKappa , endKappa , length )
```

- Three numeric scalars — κ₀, κ₁ and L — comma-separated, inside parentheses.
- Each scalar uses the same numeric literal grammar as a coordinate ordinate: signed decimal, optional exponent, with `INF` / `-INF` / `NAN` permitted by the surrounding extension grammar.
- `length > 0`. Negative length is rejected.
- `startKappa = endKappa` is rejected as degenerate; use `CIRCULARSTRING` or a line segment.

The `CLOTHOID` token is valid **only** in a non-leading `compoundCurveMember` position. Any other position is an error at parse time.

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

### 4.2 Grammar versus semantics

The grammar itself is deliberately thin. It enforces only:

- Rejection of the `CLOTHOID` token in OGC-strict mode, with the extension off.
- That the three scalars parse as numeric ordinates — which means `INF` and `NAN` are syntactically accepted here.

Everything else is a semantic-validation pass rather than a parser rule:

- `CLOTHOID` may not be the first member of a `COMPOUNDCURVE`, nor appear at top level (§3.2).
- `length` must be positive and finite, and both curvatures finite — so the `INF` / `NAN` the grammar admits are rejected at construction.
- κ₀ ≠ κ₁ (§3.10).

## 5. Grammar (landed in #4848)

Against the post-#4846 `wkt/wkt.g4`, which already had `compoundCurveGeometry` and a `compoundCurveMember` production:

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

The semantic-validation rules listed in §4.2 sit outside the grammar so that error messages can carry meaningful context, and so the ANTLR grammar itself stays decision-free.

### 5.1 Coexistence with the ISO 13249-3 clothoid

As §2 notes, ISO got there first. Subclause 5.1.67 of ISO/IEC 13249-3 defines its own `CLOTHOID`, built from an `ST_AffinePlacement` reference location plus a scale factor and start/end distances rather than from three scalars:

```wkt
CLOTHOID (REFERENCELOCATION AFFINEPLACEMENT (LOCATION (0 0), REFERENCEDIRECTIONS (VECTOR (1 0), VECTOR (0 1))), SCALEFACTOR 100, STARTDISTANCE 0, ENDDISTANCE 50)
```

Both forms are now in the grammar. They share the `CLOTHOID` token and the `clothoidGeometry` rule, and are told apart by the token after the opening parenthesis: a number for the form proposed here, `REFERENCELOCATION` for the ISO one.

```antlr
clothoidText
    : LPAR referenceLocationText COMMA scaleFactorText COMMA startDistanceText COMMA endDistanceText (
        COMMA startMText COMMA endMText
    )? RPAR
    | LPAR ordinate COMMA ordinate COMMA ordinate RPAR
    | EMPTY_
    ;
```

Everything this proposal specifies still applies to the (κ₀, κ₁, L) form, including the non-leading-member restriction of §3.2 and the semantic-validation rules of §4.2. The ISO form does not share that restriction: it carries its own placement, so it is a well-formed standalone curve anywhere the standard allows a curve.

## 6. Compatibility & fallback

Existing OGC WKT readers will fail on the `CLOTHOID` keyword. This is the *correct* OGC behaviour for an unknown extension keyword. The proposal does not advocate silent fallback or chord-replacement.

For interop with non-extension consumers, a `WKTWriter.writeWithFallback(Geometry, FallbackPolicy)` mode is suggested:

- `FAIL` (default): emit the geometry as-is, `CLOTHOID` and all. The name describes what happens downstream — a non-extension reader will fail on it — rather than the writer failing. Nothing is silently degraded.
- `DENSIFY`: replace each `CLOTHOID` segment with its `toLinear(tolerance)` chord polyline before emitting WKT. The result is OGC-valid: a `LINESTRING`, or a `COMPOUNDCURVE` of `LINESTRING` and `CIRCULARSTRING` members.
- `CHORD`: replace each `CLOTHOID` with a single straight line from start to end. Loses the transition.

This is implementation guidance, not part of the grammar proposal itself.

## 7. Implementation status

The reference Java implementation is **complete** on the JTS playground branch
[`feature/sfa-curve-clothoid-playground`](https://github.com/grootstebozewolf/jts/tree/feature/sfa-curve-clothoid-playground).
The structure mirrors how `CircularString` / `CompoundCurve` are laid out:

- **`ClothoidSegment`** (geometry) — extends `LineString`, stores `(startPoint, startTangent, startKappa, endKappa, length)`. Implements `Linearizable.toLinear(tolerance)` via §3.5.
- **`CompoundCurve` with structural members** — accepts `ClothoidSegment` alongside `LineString` / `CircularString` members; flat coordinate sequence is the dedup-concatenation of member coordinates (§3.6).
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

The SFA Curve Awareness epic ([locationtech/jts#1195](https://github.com/locationtech/jts/issues/1195)) is about preserving *spec-defined* curve types (`CIRCULARSTRING`, `COMPOUNDCURVE`, `CURVEPOLYGON`, etc.) through every JTS algorithm. **This proposal is explicitly out of scope for that epic**, and was carved out as a footnote there for exactly this reason.

`CLOTHOID` is a separate, sibling effort:
- Different precedent (civil engineering, not GIS).
- Different spec position: ISO 13249-3 defines a clothoid (§5.1), but OGC SFA does not, so JTS would be leading on the SFA side.
- Different consumers (rail/road/CAD tooling).

The two efforts share infrastructure (`CompoundCurve` member-list architecture, `Linearizable` interface, `CurvedShapeWriter` rendering pipeline), so the SFA work *unblocks* this one. It must not block on it. Land SFA first.

## 9. Open questions / future work

- **Multi-segment clothoid splines.** Real-world transitions sometimes use back-to-back clothoid segments to achieve C³ continuity (jerk-smooth). The grammar permits this — several consecutive `CLOTHOID(...)` members are well-defined — so no special syntax is needed. Document as a usage pattern in v1.
- **Top-level / leading clothoid.** Deferred to v2 if needed (see §3.2). Likely the six-scalar `CLOTHOID(x₀ y₀, θ₀, κ₀, κ₁, L)` form, in first position only.
- **Alignment with the ISO form.** §5.1 leaves both parametrisations in the grammar. Whether the JTS geometry type should also read and write the ISO spelling, and whether `ST_AffinePlacement` is worth modelling in JTS to do so, is open.
- **`ClothoidString`** (analogue of `CircularString` — a chain of clothoid arcs sharing endpoints). Possible v3; covers the rare case of a continuous clothoid sequence not interrupted by circles or straights.
- **Z / M ordinates.** The proposal treats clothoid as 2-D. Z/M handling is the same as for arcs today: propagate from start and end, do not interpolate. Document.
- **Analytical clothoid–clothoid intersection.** Non-elementary (involves intersections of Fresnel-defined curves). Deferred to *much* later; for v1, intersections are computed via `toLinear` densification and the polyline machinery.

## 10. References

- OGC Simple Feature Access 1.2.1 / ISO 19125 — for what WKT does and does not define.
- ISO/IEC 13249-3 (SQL/MM Part 3), subclause 5.1.67 — the ISO clothoid discussed in §5.1.
- IFC 4.3 — `IfcClothoid` type and parametrisation.
- LandXML 1.2 — `<Spiral spiType="clothoid" ...>` element.
- Heald, M. A. (1985). "Rational approximations for the Fresnel integrals." *Mathematics of Computation*, 44(170), 459–461.
- Cornu, M. A. (1874). "Méthode nouvelle pour la discussion des problèmes de diffraction." (Origin of the name "Cornu spiral".)
- The clothoid path-planning literature in *IEEE Transactions on Intelligent Transportation Systems* is a useful source for densification-error budgets, though no single paper is canonical here.
