module Android.AdbWrapper.Types
    ( DeviceName
    , Device(..)
    ) where

type DeviceName = String

data Device = Device
    { deviceName    :: DeviceName
    , deviceProduct :: String
    , deviceModel   :: String
    , deviceType    :: String
    } deriving (Show,Eq)

