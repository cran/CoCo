"enterQlist" <-
function (set, list, setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    if (setslot) 
        .set.coco.value(object, ".structure", append = TRUE, 
            list(type = "q.list", set = set, list = list))
    ok.coco(call.coco(107, 1, arg.char = set, arg.long = list, 
        object = object))
}
