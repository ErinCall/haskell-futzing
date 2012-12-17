import System.Environment
import System.Directory
import System.IO
import Control.Exception
import qualified Data.ByteString.Lazy as ByteString

main = do
    (source:dest:_) <- getArgs
    copy source dest

copy :: String -> String -> IO ()
copy source dest = do
    contents <- ByteString.readFile source
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            ByteString.hPutStr tempHandle contents
            hClose tempHandle
            removeFile dest
            renameFile tempName dest)
