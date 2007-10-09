"editModel" <-
function (action = NULL, modification = NULL, model = FALSE, 
    result.form = "maximal-interaction-terms", omit.test = TRUE, 
    make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
    edges = TRUE, dispose = FALSE, data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    ".normal.to.dual" <- function(omit.test = FALSE, model = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        if (omit.test) 
            .omit.test(object = object)
        coco.simple.model(125, FALSE, model, object = object)
        return(TRUE)
    }
    ".dual.to.normal" <- function(omit.test = FALSE, model = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        if (omit.test) 
            .omit.test(object = object)
        coco.simple.model(126, FALSE, model, object = object)
        return(TRUE)
    }
    ".collaps.model" <- function(set = "*", omit.test = FALSE, 
        model = FALSE, marginalize.model = FALSE, data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        if (omit.test) 
            .omit.test(object = object)
        old.current <- .before.set.current(model, object = object)
        .names <- .return.name.list.string(object = object)
        .arg.char <- paste(c(set, ";", names, ";"), collapse = "")
        result <- call.coco(129, sub.code = -ifelse(marginalize.model, 
            2, 1), arg.char = .arg.char, arg.long = c(7, 8), 
            object = object)
        .after.set.current(old.current, result, type = "long.true", 
            model = ifelse(.encode.model(model) < 0, FALSE, model), 
            object = object)
    }
    ".sub.edit" <- function(code, string, sub.code, omit.test = FALSE, 
        model = FALSE, eliminate = TRUE, make.model = FALSE, 
        return.test = FALSE, push.pop = FALSE, data = NULL,
        object = .object.of.model(model, data = data, ...), ...) {
        type <- .return.type(object)
        if (omit.test) 
            if (type == 2) 
                sub.code <- sub.code + 4096
            else .omit.test(object = object)
        if (return.test) 
            coco.string.double(code, string, sub.code = sub.code, 
                model = model, model.2 = model, eliminate = eliminate, 
                make.model = make.model, return.test = TRUE, 
                push.pop = push.pop, type = "long.and.double", 
                object = object)
        else coco.string.model(code, string, sub.code = sub.code, 
            model = model, make.model = make.model, push.pop = push.pop, 
            type = "long.true", object = object)
    }
    ".generate.decomposable" <- function(omit.test = FALSE, model = FALSE, 
        data = NULL,
        object = .object.of.model(model, data = data, ...), ...) {
        .sub.edit(180, "@", sub.code = FALSE, omit.test = omit.test, 
            model = model, eliminate = FALSE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".generate.graphical" <- function(omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL,
        object = .object.of.model(model, data = data, ...), ...) {
        .sub.edit(181, "@", sub.code = FALSE, omit.test = omit.test, 
            model = model, eliminate = FALSE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".generate.skeleton" <- function(omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        result <- .sub.edit(190, "@", sub.code = 2, omit.test = omit.test, 
            model = model, eliminate = FALSE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
        if (!(return.test)) # or 'return.model' ?
            return(FALSE)
        else return(result)
    }
    ".generate.moral" <- function(omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        result <- .sub.edit(190, "@", sub.code = 3, omit.test = omit.test, 
            model = model, eliminate = FALSE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
        if (!(return.test)) # or 'return.model' ?
            return(FALSE)
        else return(result)
    }
    ".drop.factor" <- function(factor, omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        .sub.edit(182, factor, sub.code = 0, omit.test = omit.test, 
            model = model, eliminate = TRUE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".drop.edges" <- function(gc, omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        .sub.edit(183, gc, sub.code = 0, omit.test = omit.test, 
            model = model, eliminate = TRUE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".add.edges" <- function(gc, omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        .sub.edit(184, gc, sub.code = 0, omit.test = omit.test, 
            model = model, eliminate = FALSE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".drop.interactions" <- function(gc, omit.test = FALSE, model = FALSE, 
        alternative.action = "", type = 0, make.model = FALSE, 
        return.test = FALSE, push.pop = FALSE, data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        sub.code <- .encode(c("all", "discrete", "linear", "quadratic"), 
            type, 0:3, 0)
        code <- ifelse((alternative.action == "reduce.generator"), 
            187, ifelse((alternative.action == "remove.generator"), 
                188, ifelse((alternative.action == "remove.total.interaction"), 
                  189, 185)))
        .sub.edit(code, gc, sub.code = sub.code, omit.test = omit.test, 
            model = model, eliminate = TRUE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".add.interactions" <- function(gc, omit.test = FALSE, model = FALSE, 
        make.model = FALSE, return.test = FALSE, push.pop = FALSE, 
        data = NULL, 
        object = .object.of.model(model, data = data, ...), ...) {
        .sub.edit(186, gc, sub.code = 0, omit.test = omit.test, 
            model = model, eliminate = FALSE, make.model = make.model, 
            return.test = return.test, push.pop = push.pop, object = object)
    }
    ".meet.of.models" <- function(model.1 = "current", model.2 = "base", 
        omit.test = FALSE, dispose = FALSE, data = NULL, 
        object = .object.of.models(model.1, model.2, data = data, ...), ...) {
        if (omit.test) 
            .omit.test(object = object)
        coco.string.double(177, "@", FALSE, model.1, model.2, 
            type = "long.true", object = object)
    }
    ".join.of.models" <- function(model.1 = "current", model.2 = "base", 
        omit.test = FALSE, dispose = FALSE, data = NULL, 
        object = .object.of.models(model.1, model.2, data = data, ...), ...) {
        if (omit.test) 
            .omit.test(object = object)
        coco.string.double(178, "@", FALSE, model.1, model.2, 
            type = "long.true", object = object)
    }
    ".difference.of.models" <- function(model.1 = "current", 
        model.2 = "base", edges = FALSE, dispose = TRUE, data = NULL, 
        object = .object.of.models(model.1, model.2, data = data, ...), ...) {
        model.1 <- .recover.model(model.1)
        model.2 <- .recover.model(model.2)
        old <- .before.set.both(model.1, model.2, object = object)
        if (!.is.nil.model(model.1) && .is.nil.model(model.2)) 
            makeBase(old$current, object = object)
        result <- call.coco(179, ifelse(edges, 1, 2), arg.long = c(7, 
            8), object = object)
        type <- "long.true"
        if (!.is.nil.model(old$base)) 
            makeBase(old$base, object = object)
        .after.set.current(old$current, result, type = type, 
            model = ifelse(.encode.model(model.1) < 0, FALSE, 
                model.1), object = object)
    }
    result <- NULL
    if (action == "marginal.model") 
        result <- .collaps.model(set = ifelse(length(modification) == 
            0, "*", modification), omit.test = omit.test, model = model, 
            marginalize.model = TRUE, object = object)
    else if (action == "collaps.model") 
        result <- .collaps.model(set = ifelse(length(modification) == 
            0, "*", modification), omit.test = omit.test, model = model, 
            object = object)
    else if (action == "generate.decomposable") 
        result <- .generate.decomposable(omit.test = omit.test, 
            model = model, object = object)
    else if (action == "generate.graphical") 
        result <- .generate.graphical(omit.test = omit.test, 
            model = model, object = object)
    else if (action == "generate.skeleton") 
        result <- .generate.skeleton(omit.test = omit.test, model = model, 
            object = object)
    else if (action == "generate.moral") 
        result <- .generate.moral(omit.test = omit.test, model = model, 
            object = object)
    else if (action == "dual.to.normal") 
        result <- .dual.to.normal(omit.test = omit.test, model = model, 
            object = object)
    else if (action == "normal.to.dual") 
        result <- .normal.to.dual(omit.test = omit.test, model = model, 
            object = object)
    else if (action == "drop.factor") 
        result <- .drop.factor(modification, omit.test = omit.test, 
            model = model, make.model = make.model, return.test = return.test, 
            push.pop = push.pop, object = object)
    else if (action == "drop.edges") 
        result <- .drop.edges(modification, omit.test = omit.test, 
            model = model, make.model = make.model, return.test = return.test, 
            push.pop = push.pop, object = object)
    else if (action == "add.edges") 
        result <- .add.edges(modification, omit.test = omit.test, 
            model = model, make.model = make.model, return.test = return.test, 
            push.pop = push.pop, object = object)
    else if (action == "drop.interactions") 
        result <- .drop.interactions(modification, omit.test = omit.test, 
            model = model, make.model = make.model, return.test = return.test, 
            push.pop = push.pop, object = object)
    else if (action == "add.interactions") 
        result <- .add.interactions(modification, omit.test = omit.test, 
            model = model, make.model = make.model, return.test = return.test, 
            push.pop = push.pop, object = object)
    else if (action == "reduce.generator") 
        result <- .drop.interactions(modification, omit.test = omit.test, 
            alternative.action = "reduce.generator", model = model, 
            object = object)
    else if (action == "remove.generator") 
        result <- .drop.interactions(modification, omit.test = omit.test, 
            alternative.action = "remove.generator", model = model, 
            object = object)
    else if (action == "remove.total.interaction") 
        result <- .drop.interactions(modification, omit.test = omit.test, 
            alternative.action = "remove.total.interaction", 
            model = model, object = object)
    else if ((action == "meet.of.models") || (action == "intersection")) 
        result <- .meet.of.models(model.1 = .false.if.NULL(model), 
            model.2 = .false.if.NULL(modification), dispose = dispose, 
            omit.test = omit.test, object = object)
    else if ((action == "join.of.models") || (action == "union")) 
        result <- .join.of.models(model.1 = .false.if.NULL(model), 
            model.2 = .false.if.NULL(modification), dispose = dispose, 
            omit.test = omit.test, object = object)
    else if (action == "difference.of.models") 
        result <- .difference.of.models(model.1 = .false.if.NULL(model), 
            model.2 = .false.if.NULL(modification), edges = edges, 
            dispose = dispose, object = object)
    else if (action == "decompose.models") 
        result <- .decompose.models(set = modification, model.1 = .false.if.NULL(model), 
            model.2 = FALSE, object = object)
    if (length(result) > 0) 
        if (dispose && result) {
            result <- returnModel("last", object = object)
            disposeOfModel("last", object = object)
        }
    result
}
