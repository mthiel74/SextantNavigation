# Celestial Navigation — Wolfram Language

A Wolfram Community post demonstrating classical celestial (sextant) navigation implemented in
Wolfram Language. The post walks through the full pipeline: sight reduction from raw sextant
altitude angles, intercept method (Marcq Saint-Hilaire), running fix, Monte-Carlo accuracy
analysis, and a worked 24-day Atlantic-crossing example — all computed offline in Wolfram Language
using a self-contained engine with embedded data.

## Why offline / self-contained?

Wolfram's `AstronomicalData`/`AstroValue` functions require a Wolfram server connection for star
ephemerides and are therefore unavailable in an isolated scripting environment. The engine instead
uses:

- **Sun**: IAU-1976 GMST formula + Astronomical-Almanac low-precision solar coordinates (~0.01°
  accuracy), sufficient for navigation to ~0.1 nm.
- **Stars**: embedded 58-star navigational catalogue (`data/nav_stars.csv`) with J2000 positions,
  proper motions, and magnitudes from public-domain sources (Hipparcos / Bright Star Catalogue).
  IAU-1976 precession applied at runtime to the observation epoch.
- **Voyage**: a SYNTHETIC great-circle dead-reckoning model (28°N→14°N Atlantic, Nov–Dec 2024,
  constant 5 kt, ~260° course). All sights are simulated with Gaussian noise (σ = 1 arcmin) on
  top of computed true altitudes. There is no real voyage or real sextant data.

`SunPosition[]` (built-in, offline) and `CountryData[]` are used only for cross-check figures
and map backgrounds — not for navigation computations.

An optional JPL Horizons validation script (`wolfram/fetch_horizons.wls`) cross-checks solar
positions against the online Horizons service and requires network access; it is excluded from
the main offline pipeline.

## Accuracy statement

The Monte-Carlo circular error probable (CEP ~1.1 nm) is a **random-noise floor** computed under
a perfect ephemeris: the same ephemeris generates and reduces every sight, so almanac errors,
proper-motion residuals, semi-diameter errors, and systematic biases all cancel exactly. Real-world
celestial fixes are dominated by systematic errors (refraction uncertainty, dip, index error,
chronometer drift) that do not average out over many sights. Skilled navigators typically achieve
1–3 nm in practice.

## File structure

| Path | Contents |
|------|----------|
| `wolfram/CelestialNavigation.wl` | Main package — offline, self-contained |
| `tests/run_tests.wls` | Test runner (runs all `*_tests.wl` files) |
| `wolfram/sights.wls` | Per-day Sun sight simulation + running fix |
| `wolfram/twilight_replay.wls` | Evening twilight star fixes (real catalogue) |
| `wolfram/figures.wls` | Static educational figures (9 figures) |
| `wolfram/error_budget.wls` | Error-budget dashboard |
| `wolfram/crlb.wls` | Fisher information / Cramér-Rao bound figure |
| `wolfram/cockedhat.wls` | Cocked-hat 25% theorem demonstration |
| `data/nav_stars.csv` | Embedded 58-star J2000 navigational catalogue |
| `data/sights.json` | Per-day Sun running-fix results (generated) |
| `data/twilight_fixes.json` | Per-day twilight star-fix results (generated) |
| `data/voyage.csv` | Synthetic voyage waypoints |
| `docs/images/` | Figures for the Community post |
| `community/` | Final Community post notebook |

## Reproducing

Regenerate data:

```bash
wolframscript -file wolfram/sights.wls
wolframscript -file wolfram/twilight_replay.wls
```

Render figures:

```bash
wolframscript -file wolfram/figures.wls
wolframscript -file wolfram/error_budget.wls
wolframscript -file wolfram/crlb.wls
wolframscript -file wolfram/cockedhat.wls
```

Run the test suite:

```bash
wolframscript -file tests/run_tests.wls
```

## Data & acknowledgements

- **IAU 1976 GMST formula** (public domain) — International Astronomical Union, used for
  Greenwich Mean Sidereal Time computation.
- **Astronomical Almanac low-precision solar coordinates** (public domain) — U.S. Naval
  Observatory / HM Nautical Almanac Office, used for Sun geographic position.
- **Hipparcos / Bright Star Catalogue** (public domain) — ESA / Yale University, source for the
  embedded 58-star navigational catalogue positions and proper motions.
- **JPL Horizons** (public domain) — NASA/JPL, used in the optional network-validation script
  (`wolfram/fetch_horizons.wls`) only; not part of the main offline engine.
