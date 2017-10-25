import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
  (src:dst:_) <- getArgs
  cp src dst

cp :: FilePath -> FilePath -> IO ()
cp src dst = do
  contents <- B.readFile src
  B.writeFile dst contents
