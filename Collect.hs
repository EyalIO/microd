{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Collect names in a scope as a first pass prior to semantic analysis

module Collect where

import AST
import Control.Lens
import Control.Monad.State
import Data.Map

data CollectEnv = CollectEnv
    { _collectFuncs :: Map Ident FFuncDecl
    , _collectPragmaMsgs :: [FExpr]
    }
makeLenses ''CollectEnv

collect :: Module FExpr -> CollectEnv
collect (Module _ decls) =
    mapM_ collectDecl decls `execState` CollectEnv mempty []

collectDecl :: FDecl -> State CollectEnv ()
collectDecl (DeclFunc func)      = collectFuncs . at (funcIdent func) ?= func
collectDecl (DeclPragmaMsg expr) = collectPragmaMsgs %= (expr:)

