"emOn" <-
function (hit = FALSE, setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    if (("what" != hit) && setslot) 
        .set.coco.value(object, ".structure", append = TRUE, 
            list(type = "em.on", action = hit, when = "ultimo"))
    if ((hit == "what")) 
        .set.switch("em", hit, object = object)
    else coco.simple.command(112, FALSE, object = object)
}
