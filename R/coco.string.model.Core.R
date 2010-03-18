"coco.string.model" <-
function (code, argument = FALSE, sub.code = FALSE, model = FALSE, 
    make.model = FALSE, push.pop = FALSE, type = "unconditioned", 
    data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    coco.type <- .return.type(object)
    push.pop <- push.pop && !is.character(model)
    push.pop <- push.pop && (coco.type == 2) && (make.model)
    old.current <- .before.set.current(model, push.pop = push.pop, 
        object = object)
    if (type == "long.true") 
        result <- call.coco(code, sub.code = sub.code, arg.char = argument, 
            arg.long = c(7, 8), object = object)
    else result <- coco.enter.string(code, argument, sub.code, 
        object = object)
    if (make.model) 
        result <- makeModel("last", push.pop = push.pop, 
            object = object)
    .after.set.current(old.current, result, ifelse(make.model, 
        "unconditioned", type), model = FALSE, push.pop = push.pop, 
        object = object)
}
