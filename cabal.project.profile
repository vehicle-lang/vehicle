-- Cabal project configuration file for PROFILING
--
-- Use `cabal` with `--project-file=cabal.project.profile`
-- or run vehicle via `./scripts/vehicle-profile`.

import: cabal.project

package vehicle-syntax
  ghc-options:
    -- Necessary for eventlog support
    -eventlog

    -- Necessary for info table profiling
    -finfo-table-map -fdistinct-constructor-tables

package vehicle
  ghc-options:
    -- Necessary for eventlog support
    -eventlog

    -- Necessary for info table profiling
    -finfo-table-map -fdistinct-constructor-tables

    -- Necessary on _executable_ for eventlog support
    -threaded

    -- Necessary on _executable_ for -h* RTS options
    -rtsopts
