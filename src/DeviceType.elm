module DeviceType exposing (DeviceType(..), deviceType, isMobile)


type DeviceType
    = Mobile
    | Desktop


deviceType : { width : Int, height : Int } -> DeviceType
deviceType windowSize =
    if windowSize.height > windowSize.width then
        Mobile

    else
        Desktop


isMobile : DeviceType -> Bool
isMobile deviceType_ =
    deviceType_ == Mobile
