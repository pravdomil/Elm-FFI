module Cli.Patch exposing (..)


apply : String -> String
apply a =
    a
        |> String.replace
            "var task = function (arg) {\n\t\t\treturn $elm$core$Task$fail($author$project$Interop$JavaScript$FileNotPatched);\n\t\t};"
            fn
        |> String.replace
            "var task = F2(\n\t\t\tfunction (arg, arg2) {\n\t\t\t\treturn $elm$core$Task$fail($author$project$Interop$JavaScript$FileNotPatched);\n\t\t\t});"
            fn2


fn : String
fn =
    """var fn = new Function('a', 'return ' + code);

       var task = function(arg) {
         return _Scheduler_binding(function(callback) {
           var result;
           try       { result = _Scheduler_succeed(_Json_wrap(fn(_Json_unwrap(arg)))) }
           catch (e) { result = _Scheduler_fail   ($author$project$Interop$JavaScript$Exception(_Json_wrap(e))) }
           callback(result)
         })
       };
    """


fn2 : String
fn2 =
    """var fn = new Function('a', 'b', 'return ' + code);

       var task = F2(function(arg, arg2) {
         return _Scheduler_binding(function(callback) {
           var result;
           try       { result = _Scheduler_succeed(_Json_wrap(fn(_Json_unwrap(arg), _Json_unwrap(arg2)))) }
           catch (e) { result = _Scheduler_fail   ($author$project$Interop$JavaScript$Exception(_Json_wrap(e))) }
           callback(result)
         })
       });
    """
