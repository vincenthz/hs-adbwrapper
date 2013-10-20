module Android.AdbWrapper
    ( shell
    , listDevices
    , getDirectoryContents
    , pull
    , push
    -- * Types
    , Device(..)
    , DeviceName
    ) where

import Control.Applicative
import Control.Monad (void)
import Android.AdbWrapper.Types
import Android.AdbWrapper.Lowlevel
import Android.AdbWrapper.Utils
import Data.List (isPrefixOf)

-- | execute a command on the target device
shell :: DeviceName -> [String] -> IO String
shell dev cmds = execAdb (Just dev) $ ["shell"] ++ cmds

-- | Return the directory content on the target device
getDirectoryContents :: DeviceName -> FilePath -> IO [String]
getDirectoryContents dev dir = map stripSpaces . lines <$> shell dev ["ls", dir]

-- | Return the list of devices connected to the host
listDevices :: IO [Device]
listDevices = do
    r <- execAdb Nothing ["devices", "-l"]
    case skipTill (isPrefixOf "* ") $ filter (not . null) $ lines r of
        h:l | stripSpaces h == "List of devices attached" -> return $ map toDevice l
            | otherwise -> error ("expecting header, got: " ++ show h)
        []  -> error "wrong format"
  where toDevice l = case words l of
            devName : "device" : _ : prod : model : dev : [] ->
                Device { deviceName    = devName
                       , deviceProduct = stripStart "product:" prod
                       , deviceModel   = stripStart "model:" model
                       , deviceType    = stripStart "device:" dev
                       }
            _ -> error ("wrong device format: " ++ show l)

-- | push a @local file or dir to the @remote destination path on the target device
push :: DeviceName -- ^ Target device
     -> FilePath   -- ^ Source on the host
     -> FilePath   -- ^ Destination on the target
     -> IO ()
push dev local remote =
    void $ execAdb (Just dev) ["push", local, remote]

-- | pull a @local file to the remote destination path @dst on the target device
pull :: DeviceName -- ^ Target device
     -> FilePath   -- ^ Remote file or directory
     -> FilePath   -- ^ Local destination on host
     -> IO ()
pull dev remote local =
    void $ execAdb (Just dev) ["pull", remote, local]
