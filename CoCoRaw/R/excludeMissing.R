"excludeMissing" <-
function (hit = "flop", set = ";", setslot = TRUE, data = NULL, 
    object = .object.of.thing(data = data, ...), ...) 
{
    if (("what" != hit) && setslot) 
        .set.coco.value(object, ".structure", append = TRUE, 
            list(type = "exclude.missing", action = hit, set = set, 
                when = "ultimo"))
    if ((hit == "what")) 
        call.coco.chars(110, "what", FALSE, object = object)
    else if ((hit == "in")) 
        call.coco.chars(110, set, 4, object = object)
    else coco.simple.command(110, .encode(c("off", "flop", "on"), 
        hit, 1:3, 1), object = object)
}
