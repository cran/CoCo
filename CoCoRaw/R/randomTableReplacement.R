"randomTableReplacement" <-
function (model = "current", data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "randomTableReplacement", 
        when = "ultimo"))
    old.current <- .before.set.current(model, object = object)
    result <- coco.simple.command(109, FALSE, object = object)
    .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
}
