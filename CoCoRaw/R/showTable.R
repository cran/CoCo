"showTable" <-
function (type = "observed", set = "*", model = FALSE, random = FALSE, 
    log.transformed = FALSE, complete = FALSE, discrete.ordered = TRUE, 
    table = FALSE, matrix = TRUE, mixed = FALSE, output.form = "table", 
    data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    ".show.continuous" <- function(type = "observed", set = "*", 
        model = FALSE, random = FALSE, log.transformed = FALSE, 
        complete = FALSE, discrete.ordered = TRUE, table = FALSE, matrix = TRUE, 
        data = NULL, object = .object.of.model(model, data = data, 
            ...), ...) {
        .type <- .encode.type.and.options(type, random, log.transformed, 
            complete, discrete.ordered, uniform = FALSE, rankit = table, 
            probit = matrix)
        if ((16 < .type[1]) && (.type[1] < 32)) 
            .type[1] <- .type[1] - 16
        old.current <- .before.set.current(model, object = object)
        result <- call.coco(ifelse(discrete.ordered, 123, 121), -1, 
            arg.char = set, arg.long = .type, object = object)
        .after.set.current(old.current, result, type = "ok", 
            model = FALSE, object = object)
    }
    if (output.form == "sparse.table") 
        call.coco.chars(114, set, sub.code = 1, object = object)
    else if (output.form == "case.list") 
        coco.enter.string(118, set, FALSE, object = object)
    else if (output.form == "list.all.values") 
        coco.string.model(117, set, sub.code = FALSE, model = model, 
            type = "unconditioned", object = object)
    else {
        .type <- .encode.type.and.options(type, random, log.transformed, 
            complete, discrete.ordered, uniform = FALSE, rankit = FALSE, 
            probit = FALSE)
        if ((mixed) || ((16 < .type) && (.type < 32))) 
            .show.continuous(type, set = set, model = model, 
                random = random, log.transformed = log.transformed, 
                complete = complete, discrete.ordered = discrete.ordered, 
		table = table, matrix = matrix, object = object)
        else {
            old.current <- .before.set.current(model, object = object)
            result <- call.coco(ifelse(discrete.ordered, 113, 121), -1, 
                arg.char = set, arg.long = .type, object = object)
            .after.set.current(old.current, result, type = "ok", 
                model = FALSE, object = object)
        }
    }
}
