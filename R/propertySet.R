"propertySet" <-
function (query = "in.one.clique", set = "", set.a = "", set.b = "", 
    model = FALSE, prior.action = NULL, modification = NULL, 
    data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    if (length(prior.action) > 0) {
        if (!old.current) 
            old.current <- returnModelNumber("current", object = object)
        editModel(action = prior.action, modification = modification, 
            model = model, result.form = "2-section", object = object)
        .current(object = object)
    }
    .sub.code <- 0
    if (set.a == "") 
        .arg.char <- paste(c(set, ";"), collapse = "")
    else if (set.b == "") {
        .sub.code <- -8
        .arg.char <- paste(c(set, set.a, ";"), collapse = "")
    }
    else {
        .sub.code <- -24
        .arg.char <- paste(c(set, set.a, set.b, ";"), collapse = "")
    }
    .names <- .return.name.list.string(full = TRUE, object = object)
    .arg.char <- paste(c(.arg.char, ";;;", .names, "/", .names, 
        ";"), collapse = "")
    result <- NULL
    if ((query == "is.separator") || (query == "separator")) {
        result <- call.coco(205, sub.code = .sub.code - 2, arg.char = .arg.char, 
            arg.long = c(7, 8), object = object)
    }
    else if ((query == "is.d-separator") || (query == "d-separator")) {
        result <- call.coco(205, sub.code = .sub.code - 3, arg.char = .arg.char, 
            arg.long = c(7, 8), object = object)
    }
    else if ((query == "in.one.clique")) {
        result <- call.coco(205, sub.code = -1, arg.char = .arg.char, 
            arg.long = c(7, 8), object = object)
    }
    result <- .after.set.current(old.current, result, type = "long.true", 
        model = ifelse(.encode.model(model) < 0, ifelse(length(prior.action) == 
            0, FALSE, model), model), object = object)
    .end.temporary.object(model, data = data, ...)
    return(result)
}
