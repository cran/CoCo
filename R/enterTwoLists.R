"enterTwoLists" <-
function (discrete, continuous, accumulated = FALSE, 
    # ncol = FALSE, 
    # select.case.fun = FALSE, columns = FALSE, silent = FALSE, 
    setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
        ...), ...) 
{
    .set.coco.value(object, ".observations", list(type = "two.list", 
        discrete = discrete, continuous = continuous, 
        # ncol = ncol, select.case.fun = select.case.fun, columns = columns, 
        # silent = silent, 
        accumulated = accumulated))
    arg.long <- NULL
    if (length(discrete) > 0) 
        arg.long <- c(t(discrete))
    arg.double <- NULL
    if (length(continuous) > 0) 
        arg.double <- c(t(continuous), 0)
    result <- call.coco(105, sub.code = ifelse(accumulated, 2, 
        1), arg.long = arg.long, arg.double = arg.double, object = object, 
        char.ok = FALSE)
    if (any(is.na(arg.long)) || any(is.na(arg.double))) 
        excludeMissing("on", object = object)
    if (ok.coco(result)) 
        TRUE # c(result$arg.long, result$arg.double)
    else result
}
