"isSubmodel" <-
function (model.1 = "current", model.2 = "base", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, ...), ...) 
{
    model.1 <- .recover.model(model.1)
    model.2 <- .recover.model(model.2)
    old <- .before.set.both(model.1, model.2, object = object)
    if (!.is.nil.model(model.1) && .is.nil.model(model.2)) 
        makeBase(old$current, object = object)
    reverse <- FALSE
    result <- call.coco(204, ifelse(reverse, 1, 0),
        arg.char = paste(";"), arg.long = c(0), 
        object = object)
    if (ifelse(.encode.model(model.2) < 0, FALSE, is.gc(model.2))) 
        disposeOfModel("base", object = object)
    if (!.is.nil.model(old$base)) 
        makeBase(old$base, object = object)
    .after.set.current(old$current, result, type = "long.true", 
        model = ifelse(.encode.model(model.1) < 0, FALSE, model.1), 
        object = object)
}
