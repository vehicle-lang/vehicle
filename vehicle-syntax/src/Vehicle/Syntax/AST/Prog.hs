{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Syntax.AST.Prog where

import Control.DeepSeq (NFData)
import Control.Monad ((<$!>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Vehicle.Syntax.AST.Decl (GenericDecl)
import Vehicle.Syntax.Prelude (strict)

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype GenericProg expr
  -- | List of declarations.
  = Main [GenericDecl expr]
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData expr => NFData (GenericProg expr)
instance ToJSON expr => ToJSON (GenericProg expr)
instance FromJSON expr => FromJSON (GenericProg expr)

traverseDecls :: forall m expr1 expr2. Monad m
              => (GenericDecl expr1 -> m (GenericDecl expr2))
              -> GenericProg expr1
              -> m (GenericProg expr2)
traverseDecls f (Main ds) = Main <$!> go ds
  where
    -- NOTE: this is a hand written traversal to keep the list strict
    go :: [GenericDecl expr1] -> m [GenericDecl expr2]
    go []       = return []
    go (d : ds) = do !d' <- f d; !ds' <- go ds; return (d' : ds')
