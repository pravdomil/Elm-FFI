module Cli.Patch exposing (..)


function : String
function =
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
