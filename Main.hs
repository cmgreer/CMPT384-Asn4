import System.Environment
import System.IO (stdout, hPutStrLn, readFile)

import Parser (parse)
import Proof (PF (..), ProofConjecture (..), ProofResult (..))
import Wang (wang)

main = do
    [input] <- getArgs
    contents <- readFile input
    case parse contents of
        Just p -> case wang (Conjecture [] [p]) of
            r -> hPutStrLn stdout (show r)
        _ -> hPutStrLn stdout "Parse failure"

