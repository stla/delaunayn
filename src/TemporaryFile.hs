module TemporaryFile
  where
import           Path             (fromAbsFile, parseAbsDir, parseRelFile,
                                   (</>))
import           System.Directory (getTemporaryDirectory)

getTemporaryFile :: String -> IO FilePath
getTemporaryFile filename = do
  tempDirPath <- (>>=) getTemporaryDirectory parseAbsDir
  tempFilePath <- parseRelFile filename
  return $ fromAbsFile $ tempDirPath </> tempFilePath
