import System.Environment
import System.IO
import System.Directory
main = do
  args <- getArgs
  mapM_ encryptFile args

encryptFile :: FilePath -> IO ()
encryptFile file = openAndWriteBack file encryptContent

openAndWriteBack :: FilePath -> (String -> String) -> IO ()
openAndWriteBack file trans = do
  fHandle <- openFile file ReadMode
  content <- hGetContents fHandle
  (tempName, tempHandle) <- openTempFile "." "temp"
  hPutStr tempHandle (trans content)
  hClose fHandle
  hClose tempHandle
  removeFile file
  renameFile tempName file

encryptContent :: String -> String
encryptContent src = case splitAt 10 src of
               (_,[]) -> error "file length too short"
               (head,left) -> reverse head ++ left
