"returnVertexOrder" <-
function (model = FALSE, invers.order = FALSE, default.order = TRUE, 
    max.card = FALSE, path.order = FALSE, sub.path = FALSE, marked = paste(";"), 
    u = "", v = "", names = NULL, levels = NULL, to.factor = NULL, 
    discrete = NULL, continuous = NULL, data = NULL, object = .object.of.model(model, 
        data = data, names = names, levels = levels, to.factor = to.factor, 
        discrete = discrete, continuous = continuous, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    n <- numberVariates(full = FALSE, object = object)
    sub.code <- -2
    sub.code <- sub.code + ifelse(invers.order, -4, 0)
    sub.code <- sub.code + ifelse(default.order, -8, 0)
    sub.code <- sub.code + ifelse(max.card || sub.path, -16, 
        0)
    sub.code <- sub.code + ifelse(path.order, -32, 0)
    result <- coco.enter.all(code = 135, string = paste(c(u, 
        v, marked, ";"), collapse = ""), long = rep(0, 2 * n + 
        1), double = -1, sub.code = sub.code, object = object)
    result <- .after.set.current(old.current, result, type = "unconditioned", 
        model = ifelse(.encode.model(model) < 0, FALSE, model), 
        object = object)[[2]]
    result <- matrix(result, nrow = 2)
    .names <- .split.name.set(.return.name.list.string(object = object))
    if (invers.order || path.order) {
        if (path.order) 
            result <- result[, result[1, ] != -1]
        result <- rbind(result, .names[result[1, ] + 1])
        dimnames(result) <- list(c("Index", "Complete", "Name"), 
            NULL)
        if (path.order) 
            result <- result[c(1, 3), ]
    }
    else dimnames(result) <- list(c("Order", "Complete"), .names)
    .end.temporary.object(model, data = data, names = names, 
        discrete = discrete, continuous = continuous, ...)
    return(result)
}
