"enterQtable" <-
function (set, table, setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    if (setslot) 
        .set.coco.value(object, ".structure", append = TRUE, 
            list(type = "q.table", set = set, table = table))
    ok.coco(call.coco(106, 1, arg.char = set, arg.long = table, 
        object = object))
}
