module Main where

import System.Environment
import System.Exit
import HexRec

main = do
  args <- getArgs
  fn <- case args of
          [a] -> return a
          _   -> do printUsage
                    exitWith (ExitFailure 1)

  hs <- parseHexFile fn
  mapM_ putStrLn $ map ppHexRec hs


printUsage :: IO ()
printUsage = do
  putStrLn . unlines $
    [ "Usage:"
    , "p12dasm [HEX_FILE]"
    ]
