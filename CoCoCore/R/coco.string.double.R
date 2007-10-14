"coco.string.double" <-
function (code, argument = FALSE, sub.code = FALSE, 
    model.1 = "current", model.2 = "base", 
    eliminate = TRUE, make.model = FALSE, 
    return.test = FALSE, push.pop = FALSE, 
    type = "unconditioned", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, ...), ...) 
{
    model.1 <- .recover.model(model.1)
    model.2 <- .recover.model(model.2)
    coco.type <- .return.type(object)
    push.pop <- push.pop && !is.character(model.1) && !is.character(model.2)
    push.pop <- push.pop && (coco.type == 2) && (make.model || return.test)
    old <- .before.set.both(model.1, model.2, push.pop = push.pop, 
        object = object)
    if (!.is.nil.model(model.1) && .is.nil.model(model.2)) 
        makeBase(old$current, object = object)
    if (type == "long.true") 
        result <- call.coco(code, sub.code = sub.code, arg.char = argument, 
            arg.long = c(7, 8), object = object)
    else result <- coco.enter.string(code, argument, sub.code, 
        object = object)
    if (return.test) {
        if (!(coco.type == 2)) 
            if (eliminate) 
                makeCurrent(model = "last", object = object)
            else makeBase(model = "last", object = object)
        names.long <- c("number.of.cases", "df", "adj", "number.of.tables", 
            "f.test.df")
        names.double <- c("deviance", "e.deviance", "square", 
            "e.square", "power", "e.power", "gamma", "gamma.s", 
            "gamma.s.1", "e.gamma.1", "e.gamma.2", "df.float", 
            "f.test", "e.f.test")
        sub.code <- 1 + ifelse(push.pop, 2048 + ifelse(eliminate, 
            512, 256), 0)
        if (coco.type == 2) 
            result <- call.coco(160, sub.code = sub.code, arg.long = rep(0, 
                5), arg.double = rep(0, 14), object = object)
        else {
            result <- call.coco(160, sub.code = sub.code, arg.long = rep(0, 
                4), arg.double = rep(0, 12), object = object)
            names.long <- names.long[1:4]
            names.double <- names.double[1:12]
        }
        names(result$arg.long) <- names.long
        names(result$arg.double) <- names.double
        if (result$arg.long["df"] <= 0)
          warning(paste("Degrees of freedom:", result$arg.long["df"]))
        type <- "long.and.double"
    }
    else if (make.model) {
        result <- CoCoObjects::makeModel("last", push.pop = push.pop, 
            object = object)
        type <- "unconditioned"
    }
    if (!.is.nil.model(old$base)) 
        makeBase(old$base, object = object)
    .after.set.current(old$current, result, type = type, push.pop = push.pop, 
        model = ifelse(.encode.model(model.1) < 0, FALSE, model.1), 
        object = object)
}
