import Data.Maybe
import System.Directory
import System.IO (IOMode (ReadMode), Handle, hPutStrLn, stderr, stdout, hClose, openFile)
import System.IO.Error
import System.Environment (getArgs)
import System.Process
import System.IO.Temp
import Text.Printf (printf)
import System.Exit (ExitCode(..))
import System.Console.GetOpt

randomDirectory :: IO FilePath
randomDirectory = createTempDirectory "/tmp" "stills"

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, _) = getOpt RequireOrder [] args
  case (listToMaybe nonOptions) of
    Just arguments -> do 
         let videoPath = head nonOptions
         videoh <- openFile videoPath ReadMode
         tmpDir <- createTempDirectory "/tmp" "stills"
         result <- genStills videoh tmpDir
         case result of
           Right p -> putStrLn $ "Succesfully generated stills at: " ++ p
           Left e -> putStrLn $ "error generating video stills: " ++ e

    Nothing -> putStrLn "no video path given"

genStills :: Handle -> FilePath -> IO (Either String FilePath)
genStills videoh tmpDir = do
    let videoStdin = UseHandle videoh
        fullPath = tmpDir
    (_, _, _, pHandle) <-
      createProcess ffmpeg {std_in = videoStdin,
                            std_out = CreatePipe,
                            std_err = CreatePipe}
    exit <- waitForProcess pHandle
    hClose videoh
    return $ case exit of
      ExitSuccess ->
          Right  tmpDir
      ExitFailure code ->
          Left $ "Script exited with non-zero exit code: " ++ show code
  where
    arguments = words $ "-i - -r 3 -f image2 " ++ tmpDir ++ "/DSCF2022.AVI-%03d.jpg"
    ffmpeg = proc "ffmpeg" arguments
