"returnTest" <-
function (model.1 = "current", model.2 = "base", push.pop = FALSE, 
    data = NULL, object = .object.of.models(model.1, model.2, 
        data = data, ...), ...) 
{
    model.1 <- .recover.model(model.1)
    model.2 <- .recover.model(model.2)
    type <- .return.type(object)
    push.pop <- push.pop && is.numeric(model.1) && is.numeric(model.2)
    push.pop <- push.pop && (type == 2)
    old <- .before.set.both(model.1, model.2, push.pop = push.pop, 
        object = object)
    names.long <- c("number.of.cases", "df", "adj", "number.of.tables", 
        "f.test.df")
    names.double <- c("deviance", "e.deviance", "square", "e.square", 
        "power", "e.power", "gamma", "gamma.s", "gamma.s.1", 
        "e.gamma.1", "e.gamma.2", "df.float", "f.test", "e.f.test")
    if (type == 2) 
        result <- call.coco(160, 1 + ifelse(push.pop, 2048, 0), 
            arg.long = rep(0, 5), arg.double = rep(0, 14), object = object)
    else {
        result <- call.coco(160, 1 + ifelse(push.pop, 2048, 0), 
            arg.long = rep(0, 4), arg.double = rep(0, 12), object = object)
        names.long <- names.long[1:4]
        names.double <- names.double[1:12]
    }
    names(result$arg.long) <- names.long
    names(result$arg.double) <- names.double
    if (ifelse(.encode.model(model.2) < 0, FALSE, is.gc(model.2))) 
        disposeOfModel("base", object = object)
    if (old$base) 
        makeBase(old$base, object = object)
    .after.set.current(old$current, result, type = "long.and.double", 
        push.pop = push.pop, model = ifelse(.encode.model(model.1) < 
            0, FALSE, model.1), object = object)
}
