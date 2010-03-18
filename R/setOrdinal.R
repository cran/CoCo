"setOrdinal" <-
function (set = "*", setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    if (("what" != set) && setslot) 
        .set.coco.value(object, ".structure", append = TRUE, 
            list(type = "ordinal", set = set, when = "ultimo"))
    call.coco.chars(176, set, FALSE, object = object)
}
