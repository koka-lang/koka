-----------------------------------------------------------------------------
-- The hover request handler
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module LanguageServer.Hover( hoverHandler
                           ) where

import Control.Lens                      ( (^.) )
import Control.Monad.IO.Class            ( liftIO )
import Common.Error
import Compiler.Compile                  ( compileModuleOrFile, Terminal (..) )
import Compiler.Options                  ( Flags )
import Compiler.Module                   ( loadedModule, modRangeMap )
import Data.Maybe                        ( fromJust )
import qualified Data.Text               as T
import Language.LSP.Server
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J
import LanguageServer.Conversions
import Lib.PPrint                        ( Pretty (..) )
import Syntax.Colorize                   ( signature )
import Syntax.RangeMap                   ( rangeMapFindAt, RangeInfo (..), NameInfo (..) )
import Type.Pretty                       ( defaultEnv )

hoverHandler :: Flags -> Handlers (LspM ())
hoverHandler flags = requestHandler J.STextDocumentHover $ \req responder -> do
  let J.HoverParams doc pos _ = req ^. J.params
      -- TODO: Use the VFS and recompile when the user types
      --       (instead of on every hover)
      uri = doc ^. J.uri
      -- TODO: Handle error
      filePath = fromJust $ J.uriToFilePath uri
  -- TODO: Generate diagnostics from compilation results,
  --       ideally in conjunction with the VFS (see above)
  loaded <- liftIO $ compileModuleOrFile terminal flags [] filePath False
  let rsp = do
              -- TODO: Handle errors
              let l = fst $ fromRight' $ checkError loaded
              rmap <- modRangeMap $ loadedModule l
              (r, rinfo) <- rangeMapFindAt (fromLspPos uri pos) rmap
              -- TODO: Improve rendering of the range info
              let hc = J.HoverContents $ J.markedUpContent "koka" $ T.pack $ formatHoverContents rinfo
                  hover = J.Hover hc $ Just $ toLspRange r
              return hover
  responder $ Right rsp

formatHoverContents :: RangeInfo -> String
formatHoverContents rinfo = case rinfo of
  Id qname info isdef -> show (pretty qname) ++ " : " ++ case info of
    NIValue tp  -> show $ pretty tp
    NICon tp    -> show $ pretty tp
    NITypeCon k -> show $ pretty k
    NITypeVar k -> show $ pretty k
    NIModule    -> "module"
    NIKind      -> "kind"
  Decl s name mname   -> s ++ " " ++ show (pretty name)
  Block s             -> s
  Error doc           -> "Error: " ++ show doc
  Warning doc         -> "Warning: " ++ show doc

fromRight' :: Show a => Either a b -> b
fromRight' (Right x) = x
fromRight' (Left e)  = error $ "fromRight' failed: " ++ show e

-- TODO: Emit messages via LSP's logging mechanism
terminal :: Terminal
terminal = Terminal
  { termError = const $ return ()
  , termPhase = const $ return ()
  , termPhaseDoc = const $ return ()
  , termType = const $ return ()
  , termDoc = const $ return ()
  }
