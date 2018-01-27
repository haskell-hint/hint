-- | In this module we intend to export some internal functions.
--
-- __Important note__: the authors of this library imply no assurance whatsoever
-- of the stability or functionality of the API exposed here, and compatibility
-- may break even by minor version changes. Rely on these at your
-- own risk.
--
-- The reason for showing them here is to aid discoverability
-- of already written code and prevent having to reinvent the wheel from
-- scratch if said wheel is already invented.
--
-- In case you find something here especially useful, please submit
-- an issue or a pull request at https://github.com/mvdan/hint so
-- we can discuss making it part of the official public API.
--
-- Some further context can be found here:
-- https://github.com/mvdan/hint/pull/48#issuecomment-358722638



module Hint.Internal (
    onCompilationError
) where

import Hint.Typecheck (onCompilationError)



-- todo: Consider refactoring like the following when
--       https://github.com/haskell/haddock/issues/563 is fixed
--
-- module Hint.Internal (module ReExport) where
-- import Hint.Typecheck as ReExport (onCompilationError)
