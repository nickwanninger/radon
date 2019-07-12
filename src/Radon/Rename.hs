module Radon.Rename where

import Radon.Syntax

-- API for the ast renamer for a module
runRename :: Module -> Module
runRename = id
