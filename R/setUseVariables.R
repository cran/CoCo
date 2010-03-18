"setUseVariables" <-
function (hit = "all", set = ";", setslot = FALSE, data = NULL, 
    object = .object.of.thing(data = data, ...), ...) 
{
    if ("what" != hit) 
        .set.coco.value(object, ".medio", append = TRUE, list(type = "set.read", 
            action = hit, set = set))
    if (hit == "what") 
        call.coco.chars(92, "what", FALSE, object = object)
    else if (hit == "all") 
        coco.simple.command(92, 1, object = object)
    else if (set == ";") 
        call.coco.chars(92, hit, FALSE, object = object)
    else call.coco.chars(92, set, FALSE, object = object)
}
