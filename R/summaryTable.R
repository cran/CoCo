"summaryTable" <-
function (type = "observed", set = "*", model = FALSE, random = FALSE, 
    log.transformed = FALSE, complete = FALSE, uniform = FALSE, 
    rankit = FALSE, probit = FALSE, data = NULL, object = .object.of.model(model, 
        data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    .type <- .encode.type.and.options(type, random, log.transformed, 
        complete, permuted = FALSE, uniform, rankit, probit)
    result <- call.coco(115, -1, arg.char = set, arg.long = .type, 
        object = object)
    .after.set.current(old.current, result, type = "ok", model = FALSE, 
        object = object)
}
