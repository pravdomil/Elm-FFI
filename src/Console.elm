module Console exposing (..)

{-| <https://developer.mozilla.org/en-US/docs/Web/API/console>
-}

import JavaScript
import Json.Decode
import Json.Encode
import Task
import Task.Extra


log : String -> Task.Task JavaScript.Error ()
log a =
    JavaScript.run "console.log(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


logInfo : String -> Task.Task JavaScript.Error ()
logInfo a =
    JavaScript.run "console.info(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


logWarning : String -> Task.Task JavaScript.Error ()
logWarning a =
    JavaScript.run "console.warn(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


logError : String -> Task.Task JavaScript.Error ()
logError a =
    JavaScript.run "console.error(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())



--


time : String -> Task.Task JavaScript.Error ()
time a =
    JavaScript.run "console.time(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


timeEnd : String -> Task.Task JavaScript.Error ()
timeEnd a =
    JavaScript.run "console.timeEnd(a)"
        (Json.Encode.string a)
        (Json.Decode.succeed ())


timeTask : (JavaScript.Error -> x) -> String -> Task.Task x a -> Task.Task x a
timeTask toError label a =
    time label
        |> Task.mapError toError
        |> Task.andThen
            (\_ ->
                a
                    |> Task.Extra.andAlwaysThen
                        (\x ->
                            timeEnd label
                                |> Task.mapError toError
                                |> Task.andThen
                                    (\_ ->
                                        case x of
                                            Ok x2 ->
                                                Task.succeed x2

                                            Err x2 ->
                                                Task.fail x2
                                    )
                        )
            )
