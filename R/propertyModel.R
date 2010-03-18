"propertyModel" <-
function (query = "chordal", model = FALSE, prior.action = NULL, 
    modification = NULL, data = NULL, object = .object.of.model(model, 
        data = data, ...), ...) 
{
    if ((model == "current") && (length(prior.action) == 0)) 
        old.current <- FALSE
    else {
        old.current <- .before.set.current(model, object = object)
        if (length(prior.action) > 0) {
            if (!old.current) 
                old.current <- returnModelNumber("current", object = object)
            editModel(action = prior.action, modification = modification, 
                model = model, result.form = "2-section", object = object)
            .current(object = object)
        }
    }
    if ((query == "all")) {
        graphical <- propertyModel(query = "graphical", model = "current", 
            object = object, ...)
        decomposable <- propertyModel(query = "decomposable", 
            model = "current", object = object, ...)
        tree <- propertyModel(query = "tree", model = "current", 
            object = object, ...)
        connected <- propertyModel(query = "connected", model = "current", 
            object = object, ...)
        type <- .return.type(object)
        if ((type != 2)) {
            .after.set.current(old.current, NULL, type = "unconditioned", 
                model = ifelse(.encode.model(model) < 0, ifelse(length(prior.action) == 
                  0, FALSE, model), model), object = object)
            return(list(graphical = graphical, decomposable = decomposable, 
                tree = tree, connected = connected))
        }
        else {
            mim.model <- propertyModel(query = "MIM-model", model = "current", 
                object = object, ...)
            mean.linear <- propertyModel(query = "mean-linear", 
                model = "current", object = object, ...)
            d.collapsible <- propertyModel(query = "d-collapsible", 
                model = "current", object = object, ...)
            homogeneous <- propertyModel(query = "homogeneous", 
                model = "current", object = object, ...)
            full.specified <- propertyModel(query = "full-specified", 
                model = "current", object = object, ...)
            pure.discrete <- propertyModel(query = "pure-discrete", 
                model = "current", object = object, ...)
            pure.continuous <- propertyModel(query = "pure-continuous", 
                model = "current", object = object, ...)
            mixed <- propertyModel(query = "mixed", model = "current", 
                object = object, ...)
            regression <- propertyModel(query = "regression", 
                model = "current", object = object, ...)
            .after.set.current(old.current, NULL, type = "unconditioned", 
                model = ifelse(.encode.model(model) < 0, ifelse(length(prior.action) == 
                  0, FALSE, model), model), object = object)
            return(list(graphical = graphical, decomposable = decomposable, 
                tree = tree, connected = connected, mim.model = mim.model, 
                mean.linear = mean.linear, d.collapsible = d.collapsible, 
                homogeneous = homogeneous, full.specified = full.specified, 
                pure.discrete = pure.discrete, pure.continuous = pure.continuous, 
                mixed = mixed, regression = regression))
        }
    }
    else {
        sub.code <- 99
        if ((query == "graphical")) {
            sub.code <- 1
            code <- 202
        }
        else {
            code <- 202
            if ((query == "decomposable") || (query == "chordal") || 
                (query == "triangulated") || (query == "rigid.circuit")) {
                code <- 203
                sub.code <- 2
            }
            else if ((query == "tree")) 
                sub.code <- 3
            else if ((query == "connected")) 
                sub.code <- 5
            else if ((query == "undirected") || (query == "skeleton")) 
                sub.code <- 7
            else if ((query == "moral")) 
                sub.code <- 8
            else if ((query == "acyclic")) 
                sub.code <- 9
            else if ((query == "MIM-model")) 
                sub.code <- 11
            else if ((query == "degenerated")) 
                sub.code <- 12
            else if ((query == "mean-linear")) 
                sub.code <- 13
            else if ((query == "d-collapsible")) 
                sub.code <- 14
            else if ((query == "q-equivalent")) 
                sub.code <- 15
            else if ((query == "homogeneous")) 
                sub.code <- 16
            else if ((query == "full-specified")) 
                sub.code <- 17
            else if ((query == "discrete") || (query == "pure-discrete")) 
                sub.code <- 21
            else if ((query == "continuous") || (query == "pure-continuous")) 
                sub.code <- 22
            else if ((query == "mixed")) 
                sub.code <- 23
            else if ((query == "regression") || (query == "cg-regression")) 
                sub.code <- 24
        }
        type <- .return.type(object)
        if ((sub.code == 99)) {
            warning("Invalid query")
            result <- NULL
        }
        else if ((type != 2) && (sub.code > 5)) {
            warning("Only implemented in mixed version of CoCo")
            result <- NULL
        }
        else result <- call.coco(code, sub.code, arg.char = paste(";"), 
            arg.long = c(0), object = object)
        result <- .after.set.current(old.current, result, type = "long.true", 
            model = ifelse(.encode.model(model) < 0, ifelse(length(prior.action) == 
                0, FALSE, model), model), object = object)
        .end.temporary.object(model, data = data, ...)
        return(result)
    }
}
