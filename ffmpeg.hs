import System.IO (IOMode (ReadMode), Handle, hPutStrLn, stderr, stdout, hClose, openFile)
import System.Process
import System.IO.Temp
import System.Directory
import Text.Printf (printf)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
  videoh <- openFile "DSCF2022.AVI" ReadMode
  withSystemTempDirectory "stills" $ \tmpDir -> do
    genStills videoh tmpDir
    getDirectoryContents tmpDir >>= print

genStills :: Handle -> FilePath -> IO ()
genStills videoh tmpDir = do
    let videoStdin = UseHandle videoh
        fullPath = tmpDir
    (_, _, _, pHandle) <-
      createProcess ffmpeg {std_in = videoStdin,
                            std_out = CreatePipe,
                            std_err = CreatePipe}
    exit <- waitForProcess pHandle
    hClose videoh
    case exit of
      ExitSuccess ->
        putStrLn $ "successfully wrote video stills to "  ++ tmpDir
      ExitFailure code ->
        hPutStrLn stderr $ "video2stills script exited with non-zero exit code: " ++ show code
  where
    arguments = words $ "-i - -r 3 -f image2 " ++ tmpDir ++ "/DSCF2022.AVI-%03d.jpg"
    ffmpeg = (proc "ffmpeg" arguments)


















-- TODO
-- <geekosaur> I would also note you're still doing questionable things;
--     reading from the handles should be done in separate threads, and
--     you should waitForProcess in the main thread.

