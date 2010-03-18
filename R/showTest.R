"showTest" <-
function (model.1 = "current", model.2 = "base", exact.test = NULL, 
    break.down = "", set = ";", only.if.one.edge = FALSE, data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, 
        ...), ...) 
{
    ".partitioning.test" <- function(model.1 = "current", model.2 = "base", 
        data = NULL, object = .object.of.models(model.1, model.2, 
            data = data, ...), ...) {
        coco.simple.double(164, FALSE, model.1, model.2, type = "unconditioned", 
            object = object)
    }
    ".test.one.edge" <- function(model.1 = "current", model.2 = "base", 
        data = NULL, object = .object.of.models(model.1, model.2, 
            data = data, ...), ...) {
        coco.simple.double(165, FALSE, model.1, model.2, type = "unconditioned", 
            object = object)
    }
    ".factorize" <- function(code = "edges", set = ";", model.1 = "current", 
        model.2 = "base", data = NULL, object = .object.of.models(model.1, 
            model.2, data = data, ...), ...) {
        if (code == "edges") 
            coco.string.double(166, set, 1, model.1, model.2, 
                type = "unconditioned", object = object)
        else coco.string.double(167, set, 1, model.1, model.2, 
            type = "unconditioned", object = object)
    }
    ".show.common.decompositions" <- function(model.1 = "current", 
        model.2 = "base", data = NULL, object = .object.of.models(model.1, 
            model.2, data = data, ...), ...) {
        coco.simple.double(157, FALSE, model.1, model.2, type = "unconditioned", 
            object = object)
    }
    if (length(exact.test) > 0) 
        if (is.logical(exact.test)) 
            optionsCoCo(exact.test = ifelse(exact.test, "on", "off"), object = object)
        else
            optionsCoCo(exact.test = exact.test, object = object)
    if (break.down != "") {
        if (break.down == "edges") 
            .factorize(code = "edges", set = set, model.1 = model.1, 
                model.2 = model.2, object = object)
        else if (break.down == "interactions") 
            .factorize(code = "interactions", set = set, model.1 = model.1, 
                model.2 = model.2, object = object)
        else if (break.down == "components") 
            .partitioning.test(model.1 = model.1, model.2 = model.2, 
                object = object)
        else if (break.down == "show.common.decompositions") 
            .show.common.decompositions(model.1 = model.1, model.2 = model.2, 
                object = object)
        else if (break.down == "decompose.models") 
            .decompose.models(set = set, model.1 = model.1, model.2 = model.2, 
                object = object)
    }
    else if (only.if.one.edge) 
        .test.one.edge(model.1 = model.1, model.2 = model.2, 
            object = object)
    else coco.simple.double(160, FALSE, model.1, model.2, type = "unconditioned", 
        object = object)
}
