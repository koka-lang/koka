module LanguageServer.Run( runLanguageServer
                         ) where

import Compiler.Options ( Flags )

runLanguageServer :: Flags -> [FilePath] -> IO ()
runLanguageServer flags files = putStrLn "TODO"
