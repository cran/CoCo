"coco.simple.model" <-
function (code, sub.code = FALSE, model = FALSE, 
    type = "unconditioned", data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    result <- coco.simple.command(code, sub.code, object = object)
    .after.set.current(old.current, result, type = type, 
        model = FALSE, object = object)
}
