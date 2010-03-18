"ehForceFix" <-
function (gc = "what", fix = "out", add.fix = FALSE, redo.fix = FALSE, 
    data = NULL, object = .object.of.thing(data = data, ...), 
    ...) 
{
    coco.enter.string(ifelse(redo.fix, 212, ifelse(add.fix, 210, 
        208)) + ifelse(fix == "out", 1, 0), gc, FALSE, object = object)
}
