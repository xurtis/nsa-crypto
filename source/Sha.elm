port module Sha exposing (..)

-- Port for sending out data to be hashed
port sha256 : String -> Cmd msg

-- Port for listening to hashed messages
port sha_result : (String -> msg) -> Sub msg
