# Adaptive Cruise Control — temporal safety example

An ego vehicle follows a lead vehicle.  The controller receives the initial
state `(v_ego, d_rel)` — ego speed and gap to lead vehicle — and outputs a
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

`load_specification` returns `(declarations, minimise)`.  The flag lets a
single training script run unchanged under any differentiable logic:

- `minimise = True` for loss-style logics (`DL2`, `Vehicle`): a satisfied
  property collapses to ≤ 0 and the raw value is minimised directly.
- `minimise = False` for robustness-style logics (`STL`): a satisfied
  property is ≥ 0, so the script negates the raw value before adding it to
  the total loss.

The training loop therefore uses `r if minimise else -r` for each property,
and the status printer flips its satisfaction check on the same flag.  See
the [`training.rst` reference](../../docs/training.rst) for the full
convention.
