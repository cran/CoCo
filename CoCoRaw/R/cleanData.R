"cleanData" <-
function (data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "clean.data", 
        when = "ultimo"))
    coco.simple.command(108, FALSE, object = object)
}
