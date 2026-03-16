# benchmark-histogram

Utility CLI that plots benchmark timing histograms for Vehicle builds.

## Usage

```sh
uv run --project scripts/benchmark-histogram benchmark-histogram path/to/results.json [...]
```

Use the `--commits` option to enforce a specific commit order when comparing runs.
