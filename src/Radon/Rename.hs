{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Radon.Rename where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Radon.Syntax

import Debug.Trace

newtype RenameM a =
    RenameM
        { unRenameM :: State Int a
        }
    deriving (Functor, Applicative, Monad)

type RenameMap = M.Map Name Name

type RenameState a = State RenameMap a

-- API for the ast renamer for a module
runRename :: Module -> Module
runRename mod = Module {modName = name, modStmts = renameTopDecls stmts}
  where
    name = modName mod
    stmts = modStmts mod

getTopLevelNames :: [TopDecl] -> [Name]
getTopLevelNames = foldl extractNames []

extractNames :: [Name] -> TopDecl -> [Name]
extractNames ns (Binding name _) = name : ns
extractNames ns _ = ns

renameTopDecls :: [TopDecl] -> [TopDecl]
renameTopDecls decls = map (renameTop bindings) decls
  where
    bindings = getTopLevelNames decls

-- Matches a top decl and does a rename run on it
renameTop :: [Name] -> TopDecl -> TopDecl
renameTop bindings (Binding name as) = Binding name as

-- Run rename on an arg case of a def binding
renameArgCase :: ArgCase -> ArgCase
renameArgCase = id

renameExpr :: Expr -> Expr
renameExpr e = evalState (eRename e) M.empty

-- | Run rename on an expression
eRename :: Expr -> RenameState Expr
eRename e = do
    get >>= traceShowM
    return e

findBinding :: Name -> RenameState (Maybe Name)
findBinding k = state $ \m -> (M.lookup k m, m)

addBinding :: Name -> Name -> RenameState ()
addBinding from to = state $ \m -> ((), M.insert from to m)
