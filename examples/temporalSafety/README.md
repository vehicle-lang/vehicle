# Adaptive Cruise Control — temporal safety example

An ego vehicle follows a lead vehicle.  The controller receives the initial
state `(v_ego, d_rel)` (ego speed and gap to lead vehicle) and outputs a
predicted velocity trajectory over 10 time steps.  Three Signal Temporal Logic
(STL) properties are verified over that trajectory using Vehicle's temporal
operators.

## Specification (`temporalSafety.vcl`)

| Property | Operator | Meaning |
|---|---|---|
| `alwaysBelowLimit` | `globally[0,9]` | Speed never exceeds 33 m/s at any step |
| `eventuallyAtTarget` | `finally[0,9]` | Speed reaches 28.5–31.5 m/s at some step |
| `limitUntilTarget` | `until[0,9]` | Speed stays within the hard limit until cruise is reached |

Initial state: ego at **15 m/s**, **30 m** behind the lead vehicle.
Target cruise speed: **30 m/s** (acceptable band: 28.5–31.5 m/s).

## Running

```bash
# From the repo root (requires cabal or a vehicle binary on PATH)
cd vehicle-python
uv run --extra pytorch python ../examples/temporalSafety/train.py
```

## Expected output

An untrained network outputs values near zero, so `eventuallyAtTarget` and
`limitUntilTarget` are deeply violated while `alwaysBelowLimit` is trivially
satisfied (outputs are far below 33 m/s).

After 300 training epochs all three properties should be satisfied, and the
velocity trace should show acceleration from ~15 m/s toward the 30 m/s target.
The sign convention in the printed output depends on the selected logic —
`STL` reports positive values for satisfied properties, `DL2` and `Vehicle`
report non-positive values.

```
Robustness BEFORE training  (positive = satisfied)   # STL
  alwaysBelowLimit          +33.xx  [OK      ]
  eventuallyAtTarget        -28.xx  [VIOLATED]
  limitUntilTarget          -28.xx  [VIOLATED]

Robustness AFTER training   (positive = satisfied)
  alwaysBelowLimit           +x.xx  [OK      ]
  eventuallyAtTarget         +x.xx  [OK      ]
  limitUntilTarget           +x.xx  [OK      ]

Learned velocity trace (m/s)  [target band: 28.5 – 31.5]
  step  0:  xx.xx m/s  ...
  ...
  step  9:  ~30.x m/s  ###############
```

## Logic-portable sign handling

The compiler emits each property as a minimisation target by default —
robustness-style logics (`STL`) are wrapped in `not` so reducing the
output drives the property toward satisfaction, regardless of which
logic was selected. A satisfied property reads `<= 0`. Pass
`--dl-native-direction` through to the compiler if you need the raw
DL-native form. See the [`training.rst` reference](../../docs/training.rst).
