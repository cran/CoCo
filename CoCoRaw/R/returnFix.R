"returnFix" <-
function (code = "edges", data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    if ("edges" == code) 
        .fix.edges("what", object = object)
    else if ("in" == code) 
        ehForceFix("what", fix = "in", object = object)
    else if ("out" == code) 
        ehForceFix("what", fix = "out", object = object)
}
