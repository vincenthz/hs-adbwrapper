module Android.AdbWrapper.Lowlevel where

import System.Process hiding (shell)
import System.Exit

import Android.AdbWrapper.Types (DeviceName)

-- | Execute an adb command maybe adding a specific device
-- and return the exitcode, output content and error contents
execAdbRet :: Maybe DeviceName -> [String] -> IO (ExitCode, String, String)
execAdbRet mDevName args = readProcessWithExitCode "adb" allArgs ""
  where allArgs = maybe id (\d -> \xs -> ("-s":d:xs)) mDevName $ args

-- | Execute an adb command and return the output on success
-- or raise an error with the error output and the exitcode.
execAdb :: Maybe DeviceName -> [String] -> IO String
execAdb mDevName args = do
    (ec, out, err) <- execAdbRet mDevName args
    case ec of
        ExitSuccess   -> return out
        ExitFailure i -> throwError i err

throwError :: Show a => a -> [Char] -> t
throwError ec err = error ("adb failed: exitcode=" ++ show ec ++ " err=" ++ err)
