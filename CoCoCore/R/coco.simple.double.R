"coco.simple.double" <-
function (code, sub.code = FALSE, 
    model.1 = "current", model.2 = "base", 
    type = "unconditioned", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, ...), ...) 
{
    model.1 <- .recover.model(model.1)
    model.2 <- .recover.model(model.2)
    old <- .before.set.both(model.1, model.2, object = object)
    if (!.is.nil.model(model.1) && .is.nil.model(model.2)) 
        makeBase(old$current, object = object)
    result <- coco.simple.command(code, sub.code, object = object)
    if (!.is.nil.model(old$base)) 
        makeBase(old$base, object = object)
    .after.set.current(old$current, result, type = type, 
                       model = ifelse(.encode.model(model.1) < 0, 
                                      FALSE, model.1), object = object)
}
