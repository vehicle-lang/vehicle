module Vehicle.Prelude.Git
  ( gitChangedTreeHash,
  )
where

import Data.Hashable (Hashable (..))
import Development.GitRev.Typed
import Language.Haskell.TH

-- | Compute a hash reflecting the current Git:
--     * staged changes
--     * unstaged changes (tracked files)
gitChangedTreeHash :: Q Exp
gitChangedTreeHash = do
  stagedChanges <- projectError $ runGitQ ["diff", "--name-only", "--cached"] IdxUsed
  unstagedChanges <- projectError $ runGitQ ["diff", "--name-only"] IdxUsed
  let changedFiles = lines stagedChanges <> lines unstagedChanges

  fileInfos <- projectError $ runGitQ (["ls-files", "-s"] <> changedFiles) IdxUsed
  let fileHashes = takeHash <$> lines fileInfos

  -- Would be nice to use Git's actual hashing mechanism here but...
  let finalHash = show (hash fileHashes)

  litE (stringL finalHash)

takeHash :: String -> String
takeHash s = words s !! 2
