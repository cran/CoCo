"ehSetBase" <-
function (model = FALSE, data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    if (model == "what") 
        result <- coco.enter.string(207, model, FALSE, object = object)
    else result <- coco.simple.command(228, FALSE, object = object)
    .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
}
