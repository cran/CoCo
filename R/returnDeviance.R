"returnDeviance" <-
function (model.1 = "current", model.2 = "base", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, ...), ...) 
{
    model.1 <- .recover.model(model.1)
    model.2 <- .recover.model(model.2)
    old <- .before.set.both(model.1, model.2, object = object)
    type <- .return.type(object)
    names.long <- c("dimension.current", "dimension.base", "df", 
        "expected.zeros.in.base", "observed.zeros.in.current", 
        "observed.zeros.in.base", "adjustment", "df.adjusted", 
        "t.test.df")
    names.double <- c("log.lik.current", "log.lik.base", "deviance", 
        "probability", "adjusted.probability", "f.test.statistic", 
        "f.test.p.value")
    if (type == 2) 
        result <- call.coco(162, 1, arg.long = rep(0, 9), arg.double = rep(0, 
            7), object = object)
    else {
        result <- call.coco(162, 1, arg.long = rep(0, 8), arg.double = rep(0, 
            5), object = object)
        names.long <- names.long[1:8]
        names.double <- names.double[1:5]
    }
    names(result$arg.long) <- names.long
    names(result$arg.double) <- names.double
    if (ifelse(.encode.model(model.2) < 0, FALSE, is.gc(model.2))) 
        disposeOfModel("base", object = object)
    if (old$base) 
        makeBase(old$base, object = object)
    .after.set.current(old$current, result, type = "long.and.double", 
        model = ifelse(.encode.model(model.1) < 0, FALSE, model.1), 
        object = object)
}
