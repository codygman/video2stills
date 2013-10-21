-- <geekosaur> I would also note you're still doing questionable things;
--     reading from the handles should be done in separate threads, and
--     you should waitForProcess in the main thread.

import System.IO (IOMode ( ReadMode), Handle, hPutStrLn, stderr, stdout, hClose, openFile)
import System.Process
import System.IO.Temp
import System.Directory
import System.Exit (ExitCode(..))
--import Data.ByteString

main :: IO ()
main = do
  videoh <- openFile "DSCF2022.AVI" ReadMode
  genStills videoh
  hClose videoh

genStills :: Handle -> IO ()
genStills videoh = do
  withSystemTempDirectory "stills" $ \tmpDir -> do
    let videoStdin = UseHandle videoh
    let fullFilePath = tmpDir ++ "/DSCF2022.AVI-%03d.jpg"
    (_, Just hout, Just herr, pHandle) <-
      createProcess(proc "ffmpeg" ["-i","-","-r","3","-f","image2",fullFilePath]) {std_in = videoStdin, std_out = CreatePipe, std_err = CreatePipe}
    exit <- waitForProcess pHandle
    case exit of
      ExitSuccess -> do
        isDirectory <- doesDirectoryExist tmpDir
        hPutStrLn stdout $ "successfully wrote video stills to "  ++ tmpDir 
        -- TODO: read an image to bytes and print out part of it to verify this is doing what I think it is
      ExitFailure code -> do
        hPutStrLn stderr $ "video2stills script exited with non-zero exit code: " ++ show code
    











-- loadStills takes a FilePath, reads all files contents into a bytestring, appends each bytestring to list of bytestrings
-- loadStills :: FilePath -> [ByteString]
-- loadStills = undefined



