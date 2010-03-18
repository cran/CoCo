"returnVariableDescription" <-
function (full = FALSE, levels = TRUE, missing.levels = TRUE, 
    data = NULL, object = .object.of.thing(data = data, ...), 
    ...) 
{
    name.list.string <- .return.name.list.string(full = full, 
        object = object)
    names <- .split.name.set(name.list.string)
    n <- length(names)
    factor.type.list <- .return.factor.type.list(full = full, 
        number.variates = n, object = object)
    level.list <- NULL
    missing.list <- NULL
    if (levels) {
        level.list <- .return.level.list(full = full, number.variates = n, 
            object = object)
        if (missing.levels) 
            missing.list <- .return.missing.list(full = full, 
                number.variates = n, object = object)
    }
    return(list(names = names, labels = NULL, types = factor.type.list, 
        stratum = NULL, levels = level.list, missing.levels = missing.list))
}
