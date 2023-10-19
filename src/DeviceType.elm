module DeviceType exposing (DeviceType(..), deviceType, isMobile)


type DeviceType
    = Mobile
    | Desktop


deviceType : { width : Int, height : Int } -> DeviceType
deviceType windowSize =
    if windowSize.width >= 800 then
        Desktop

    else
        Mobile


isMobile : DeviceType -> Bool
isMobile deviceType_ =
    deviceType_ == Mobile
