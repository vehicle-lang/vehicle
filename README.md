
# Vehicle

[![Tests](https://github.com/wenkokke/vehicle/actions/workflows/vehicle-tests.yml/badge.svg)](https://github.com/wenkokke/vehicle/actions/workflows/vehicle-tests.yml)

# Vehicle


## Dependencies
   - stack
   - bnfc >2.9
       - `stack update`
       - `stack install BNFC`

## Installation
   - `stack upgrade`
   - `make tests`

## Add files

   - Create a new file examples/misc/X/X.vcl where X is the name of your file

   - Add a new test in Test.Golden - test/Test/Golden.hs#L48

   ```
       miscTestList :: [GoldenTestSpec]
       miscTestList = map (addTestDirectory "./examples/misc")
       [ ("testing", [Verifier SMTLib, ITP Agda])
       ]
   ```

   Run `stack test --test-arguments "--accept -p testing"` as described in HACKING.md

   or

   `.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/vehicle/vehicle --help`


the grammar itself can be found here:
        https://github.com/wenkokke/vehicle/blob/dev/src/bnfc/Frontend.cf
