# Double-integrator reach-avoid — closed-loop safety example

A point mass on a line must be steered from the origin to a goal band
while keeping its position within safe bounds.  The controller observes
`(position, velocity)` and outputs an acceleration; a 10-step rollout
through a fixed dynamics model produces the trajectory that three STL
properties are checked over.

## Specification (`closedLoopSafety.vcl`)

| Property | Operator | Meaning |
|---|---|---|
| `stayBounded` | `globally[0,9]` | Position stays within `[0, 15]` at every step |
| `reachGoal` | `finally[0,9]` | Position enters the goal band `[9, 11]` at some step |
| `safeUntilGoal` | `until[0,9]` | Every state dimension stays within bounds until the position enters the goal band.  The body uses `forall k` over `Index 2` — see the temporal-operator reference for the composition pattern. |

Initial state: position **0.0 m**, velocity **0.0 m/s**.
Goal band: **[9, 11] m**.  Position bounds: **[0, 15] m**.

## Running

```bash
# From vehicle-python/
cd vehicle-python
uv run ../examples/closedLoopSafety/train.py
```

## Expected output

An untrained controller outputs near-zero accelerations, so the mass
stays at the origin: `stayBounded` is trivially satisfied while
`reachGoal` and `safeUntilGoal` are violated.

After 300 training epochs the controller learns an accelerate-then-
decelerate strategy that lands the position in the goal band while
keeping every state dimension inside its bounds throughout.

```
Robustness BEFORE training  (positive = satisfied)   # STL
  stayBounded       +xx.xx  [OK      ]
  reachGoal         -xx.xx  [VIOLATED]
  safeUntilGoal     -xx.xx  [VIOLATED]

Robustness AFTER training   (positive = satisfied)
  stayBounded        +x.xx  [OK      ]
  reachGoal          +x.xx  [OK      ]
  safeUntilGoal      +x.xx  [OK      ]

Learned trajectory  [goal: 9-11, bounds: 0-15]
  step     pos      vel    accel
  ...
```

A satisfied property prints `<= 0` regardless of which logic was
selected — the compiler emits each property as a minimisation target
by default (see the convention below).

## Logic-portable sign handling

The compiler emits each property as a minimisation target by default —
robustness-style logics (`STL`) are wrapped in `not` so reducing the
output drives the property toward satisfaction. The same file trains
unchanged under any differentiable logic. Pass `--dl-native-direction`
through to the compiler for the raw DL-native form. See the
[`temporalSafety` README](../temporalSafety/README.md) for the full
convention.
