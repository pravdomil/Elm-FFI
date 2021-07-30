module Cli.Patch exposing (..)


apply : String -> String
apply a =
    a
        |> String.replace
            "var $author$project$Interop$JavaScript$fn = function (_v0) {\n\treturn function (_v1) {\n\t\treturn $elm$core$Task$fail($author$project$Interop$JavaScript$FileNotPatched);\n\t};\n};"
            ("var $author$project$Interop$JavaScript$fn = " ++ fn)
        |> String.replace
            "var $author$project$Interop$JavaScript$asyncFn = function (_v0) {\n\treturn function (_v1) {\n\t\treturn $elm$core$Task$fail($author$project$Interop$JavaScript$FileNotPatched);\n\t};\n};"
            ("var $author$project$Interop$JavaScript$asyncFn = " ++ asyncFn)


fn : String
fn =
    """function(name) {
         var fn = new Function(['args'], 'return ' + name + '.apply(' + name + ', args)')

         return $author$project$Interop$JavaScript$Function(function(args) {
           return _Scheduler_binding(function(callback) {
             var result;
             try       { result = _Scheduler_succeed(_Json_wrap(fn(args))) }
             catch (e) { result = _Scheduler_fail   ($author$project$Interop$JavaScript$Exception(_Json_wrap(e))) }
             callback(result)
           })
         })
       };
    """


asyncFn : String
asyncFn =
    """function(name) {
         var fn = new Function(['args'], 'return ' + name + '.apply(' + name + ', args)')

         return $author$project$Interop$JavaScript$Function(function(args) {
           return _Scheduler_binding(async function(callback) {
             var result;
             try       { result = _Scheduler_succeed(_Json_wrap(await fn(args))) }
             catch (e) { result = _Scheduler_fail   ($author$project$Interop$JavaScript$Exception(_Json_wrap(e))) }
             callback(result)
           })
         })
       };
    """
