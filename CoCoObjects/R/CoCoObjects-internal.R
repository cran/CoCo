".clear.coco.objects" <-
function (coco.object = NULL, silent = FALSE, pos = .GlobalEnv) 
{
  clearCoCoObjects(coco.object = coco.object, silent = silent, pos = pos,
                   printWarnings = TRUE)
  if (is.null(coco.object) && !silent) {
    cat("Please call 'clearCoCoObjects' to make CoCo able \n")
    cat("to restore the CoCo objects of the workspace. \n")
  }
}
".end.temporary.object" <-
function (model, data = NULL, object = .current.coco, names = NULL, 
    discrete = NULL, continuous = NULL) 
{
    if (is.character(data) || !is.null(names) || !is.null(discrete) || 
        !is.null(continuous)) {
        if (!is.character(data) || (data != "Do.not.end")) 
            endCoCo(object, silent = TRUE)
    }
}
".First.lib" <-
function (lib, pkg) 
{
    .First.lib.CoCoObjects(lib, pkg)
}
".Last.lib" <-
function (lib, pkg) 
{
    .clear.coco.objects(silent = TRUE, pos = .GlobalEnv)
}
".onUnload" <-
function (lib, pkg) 
{
    .clear.coco.objects(silent = TRUE, pos = .GlobalEnv)
}
".First.lib.CoCoObjects" <-
function (lib, pkg) 
{
    require(methods)
    setClass("CoCoIdClass", representation(.reference = "numeric", 
        .key = "character", .type = "numeric", .title = "character"))
    setClass("CoCoClass", representation("CoCoIdClass", .parameters = "list", 
        .invalid = "list", .specification = "list", .medio = "list", 
        .observations = "list", .structure = "list"))
    setClass("CoCoModelClass", representation("CoCoIdClass", 
        .model = "character", .model.number = "numeric"))
}
".new.coco" <-
function (object = .current.coco, type = 1, uniq.title = FALSE, 
    title = "A CoCo object") 
{
    result <- object
    if (.is.nil.model(title)) 
        title <- "A CoCo object"
    identification <- .return.reference(object = object)
    if (uniq.title) 
        key <- paste(title, " / ", date())
    else key <- paste(date(), identification + runif(1))
    result <- new("CoCoClass", .reference = identification, .key = key, 
        .type = type, .parameters = list(type = NULL), .invalid = list(type = NULL), 
        .specification = list(type = NULL), .medio = list(type = NULL), 
        .observations = list(type = NULL), .structure = list(type = NULL), 
        .title = title)
    my.assign(".instances.coco", c(.instances.coco, result@.reference), 
        frame = 0)
    return(result)
}
".new.coco.model" <-
function (number, model, object = .current.coco, title = "") 
{
    result <- number
    result <- new("CoCoModelClass", .reference = .return.reference(object = object), 
        .key = .return.key(object = object), .type = .return.type(object = object), 
        .title = title, .model = model, .model.number = number)
    if (ifelse(is.character(title), title != "", title)) 
        result@.title <- title
    my.assign(".instances.coco.models", c(.instances.coco.models, 
        result@.reference), frame = 0)
    return(result)
}
".object.of.model" <-
function (model, data = NULL, object = .current.coco, names = NULL, 
    levels = NULL, to.factor = NULL, discrete = NULL, continuous = NULL) 
{
    result <- object
    .my.trace(".object.of.model,        start:", level = 20000, 
        name = "ooo", model = object, object = model)
    if (.is.nil.model(model)) 
        result <- .object.of.thing(data = data, to.factor = to.factor, 
            object = object)
    else if (class(model) == "CoCoModelClass") {
        coco.object <- model
        result <- .recover(coco.object, recover = TRUE)
    }
    else if (!is.null(continuous)) {
        result <- make.cococg(silent = TRUE)
        if (is.null(names)) 
            if (is.character(discrete) & is.character(continuous)) 
                names <- c(discrete, continuous)
            else names <- .names.from.model(model)
        if (is.null(levels)) {
            levels <- rep(1, length(names))
            if (is.numeric(continuous)) 
                levels[continuous] <- 0
            else if (is.character(continuous)) {
                if (length(continuous) == 1) {
                  if (all(is.na(match(continuous, names)))) 
                    continuous <- .split.name.set(continuous)
                }
                levels[match(continuous, names)] <- 0
            }
        }
        enterNames(names = paste(names, sep = "", collapse = ""), 
            levels = levels, object = result)
        enterTwoLists(discrete = levels[levels == 1], continuous = levels[levels != 
            1])
        enterModel("*")
    }
    else if (is.character(data) || !is.null(discrete) || !is.null(names)) {
        result <- make.coco(silent = TRUE)
        if (is.null(names)) 
            if (is.character(discrete)) 
                names <- c(discrete)
            else names <- .names.from.model(model)
        if (is.null(levels)) 
            levels <- rep(1, length(names))
        enterNames(names = paste(names, sep = "", collapse = ""), 
            levels = levels, object = result)
        enterList(levels, object = result)
        enterModel("*")
    }
    else result <- .object.of.thing(data = data, to.factor = to.factor, 
        object = object)
    .my.trace(".object.of.model,        stop:", level = 20000, 
        name = "OOO", key = -1, id = .return.reference(result), 
        number = .return.model.number(result), object = result)
    return(result)
}
".object.of.thing" <-
function (data = NULL, object = .current.coco, to.factor = NULL) 
{
    result <- object
    .my.trace(".object.of.thing,        start:", level = 20000, 
        name = "ooo", model = object)
    if (is.null(data)) {
        coco.object <- object
        result <- .recover(coco.object, recover = TRUE)
    }
    else if ((class(data) == "CoCoClass") || (class(data) == 
        "CoCoModelClass")) {
        coco.object <- data
        result <- .recover(coco.object, recover = TRUE)
    }
    else if (class(data) == "table") {
        result <- make.coco()
        enterNames(names = paste(":", names(dimnames(data)), 
            sep = "", collapse = ""), levels = dim(data), object = result)
        enterTable(data, object = result)
        enterModel("*")
    }
    else if (class(data) == "array") {
        result <- make.coco()
        enterTable(data, object = result)
        enterModel("*")
    }
    else if ((class(data) == "data.frame") || (class(data) == 
        "matrix")) {
        result <- make.cococg()
        enterDataFrame(data, to.factor = to.factor, object = result)
        enterModel("*")
    }
    else if (is.character()) {
    }
    .my.trace(".object.of.thing,        stop:", level = 20000, 
        name = "OOO", key = -1, id = .return.reference(result), 
        number = .return.model.number(result), object = result)
    return(result)
}
".onAttach" <-
function (lib, pkg) 
{
}
".onLoad" <-
function (lib, pkg) 
{
    .First.lib.CoCoObjects(lib, pkg)
}
".packageName" <-
"CoCoObjects"
".recover" <-
function (object = .current.coco, recover = FALSE) 
{
    id <- .return.reference(object = object)
    if ((!is.numeric(id)) & (id == FALSE)) 
        stop("Invalid memory reference (identification) of CoCo object")
    if ((id == .ended.coco) && recover) {
        result <- .recover.coco(object = object)
        id <- .return.reference(result)
    }
    else result <- object
    if (id == .ended.coco) 
        stop("Ended CoCo object")
    if (id == 0 || any(.coco.identifications[, 1] == id)) 
        return(result)
    else stop("Not a valid CoCoObject")
}
".recover.coco" <-
function (object, key = .return.key(object), level = 10000, pos = .GlobalEnv) 
{
    .my.trace(".recover.coco,           start:", level = level, 
        name = "aaa", key = key, id = .return.reference(object))
    new.object <- .recover.search(object, key = key, level = level, 
        pos = pos)
    if (class(object) == "CoCoModelClass") 
        if (.return.reference(object) == .ended.coco) 
            new.object <- recover.coco.model(object, level = level + 
                10, pos = pos)
    .my.trace(".recover.coco,            stop:", level = level, 
        name = "AAA", key = key, id = .return.reference(new.object), 
        number = .return.model.number(new.object), new.object)
    return(new.object)
}
".recover.model" <-
function (coco.model.object) 
{
    id <- .return.reference(coco.model.object)
    .my.trace(".recover.model,          start:", level = 10000, 
        name = "xxx", key = .return.key(coco.model.object), model = .return.model.of.object(coco.model.object), 
        id = id)
    if (class(coco.model.object) == "CoCoModelClass") 
        if (id == .ended.coco) 
            coco.model.object <- recover.coco.model(coco.model.object)
    .my.trace(".recover.model,           stop:", level = 90000, 
        name = "XXX", key = .return.key(coco.model.object), model = .return.model.of.object(coco.model.object), 
        id = .return.reference(coco.model.object), number = .return.model.number(coco.model.object), 
        object = coco.model.object)
    return(coco.model.object)
}
".recover.reference" <-
function (coco.object) 
{
    id <- .return.reference(coco.object)
    if ((id == .ended.coco)) {
        result <- .recover.coco(coco.object)
        id <- .return.reference(result)
    }
    return(id)
}
".recover.search" <-
function (coco.object, key = .return.key(coco.object), level = 10000, 
    pos = .GlobalEnv) 
{
    .my.trace(".recover.search,         start:", level = level, 
        name = "bbb", key = key, id = .return.reference(coco.object))
    Objects <- ls(all.names = TRUE, pos = pos)
    for (i in 1:length(Objects)) {
        .object <- get(Objects[i], pos = pos)
        if ((class(.object) == "CoCoClass")) 
            if ((.return.key(.object) == key)) {
                if (.return.reference(.object) == .ended.coco) {
                  cat("Recovering CoCo-object: '", Objects[i], 
                    "'.\n")
                  assign(Objects[i], .sub.recover.coco(object = .object, 
                    name = Objects[i], level = level + 1), pos = pos)
                }
                result <- get(Objects[i], pos = pos)
            }
    }
    .my.trace(".recover.search,          stop:", level = level, 
        name = "BBB", key = key, id = .return.reference(result), 
        number = .return.model.number(result), result)
    return(result)
}
".return.key" <-
function (object = .current.coco) 
{
    if (class(object) == "CoCoClass" || class(object) == "CoCoModelClass") 
        is.object <- any(slotNames(object) == ".key")
    else is.object <- FALSE
    if (is.object) 
        return(object@.key)
    else if (all(is.character(object))) 
        return(object[1])
    else return(FALSE)
}
".return.model.number" <-
function (coco.model = .current.coco) 
{
    if (class(coco.model) == "CoCoModelClass") 
        is.object <- any(slotNames(coco.model) == ".model.number")
    else is.object <- FALSE
    if (is.object) 
        return(coco.model@.model.number)
    else if (all(is.number(coco.model))) 
        return(coco.model[1])
    else return(FALSE)
}
".return.model.of.object" <-
function (model, object = .current.coco) 
{
    if (is.character(model)) 
        return(model)
    if (is(model, "CoCoModelClass")) 
        return(model@.model)
    else if (is(object, "CoCoModelClass") & (.is.nil.model(model))) 
        return(object@.model)
    else return(FALSE)
}
".return.object.model.number" <-
function (number, recover = TRUE, object = .current.coco) 
{
    if (class(number) == "CoCoModelClass") 
        y <- number@.model.number
    else y <- number
    if (class(object) == "CoCoModelClass") 
        x <- object@.model.number
    else x <- y
    id <- .return.reference(object = object)
    .my.trace(".return.object.model.number    ", level = 30000, 
        name = "OOO", key = id, model = c("<", x, ",", y, ">"), 
        number = number, object = ";")
    if (is.numeric(number)) 
        return(number)
    if (is(number, "CoCoModelClass")) {
        if (recover) 
            number <- .recover.model(number)
        return(number@.model.number)
    }
    else if (is(object, "CoCoModelClass") & (.is.nil.model(number))) {
        if (recover) 
            object <- .recover.model(object = object)
        return(object@.model.number)
    }
    else return(FALSE)
}
".return.reference" <-
function (object = .current.coco) 
{
    if (class(object) == "CoCoClass" || class(object) == "CoCoModelClass") 
        is.object <- any(slotNames(object) == ".reference")
    else is.object <- FALSE
    if (is.object) 
        return(object@.reference)
    else if (all(is.number(object))) 
        return(object[1])
    else return(FALSE)
}
".return.type" <-
function (object = .current.coco) 
{
    if (class(object) == "CoCoClass" || class(object) == "CoCoModelClass") 
        is.object <- any(slotNames(object) == ".type")
    else is.object <- FALSE
    if (is.object) 
        return(object@.type)
    else if (all(is.number(object))) 
        return(object[2])
    else return(FALSE)
}
".set.coco.value" <-
function (argument, slotid, value, pos = .GlobalEnv, append = FALSE) 
{
    result <- argument
    key <- .return.key(argument)
    Objects <- ls(all.names = TRUE, pos = pos)
    for (i in 1:length(Objects)) {
        .object <- get(Objects[i], pos = pos)
        if (class(.object) == "CoCoClass") 
            if (.return.key(.object) == key) {
                if (append) {
                  old.value <- slot(.object, slotid)
                  if (length(old.value[[1]]) > 0) 
                    value <- append(old.value, list(value))
                  else value <- list(value)
                }
                result <- assign(Objects[i], .SetSlotValue(.object, 
                  slotid, value), pos = pos)
            }
    }
    return(result)
}
".SetSlotValue" <-
function (object, slotid, value) 
{
    slot(object, slotid) <- value
    return(object)
}
".sub.recover.coco" <-
function (object, name = "", level = 1, n = 65536, p = 65536, 
    q = 1024, r = 65536, s = 65536, ss = 65536, t = 65536, location = c(700, 
        550), manager = TRUE, silent = FALSE, sh.lib.name = NULL) 
{
    .my.trace(".sub.recover.coco,       start:", level = level, 
        name = name, key = -1, id = .return.reference(object))
    P <- object@.parameters
    coco.init(P$size["n"], P$size["p"], P$size["q"], P$size["r"], 
        P$size["s"], P$size["ss"], P$size["t"], TRUE, title = object@.title, 
        type = object@.type, location = P$location, manager = P$manager, 
        sh.lib.name = P$sh.lib.name)
    new.object <- .SetSlotValue(get(name, pos = .GlobalEnv), 
        ".reference", .return.reference(.current.coco))
    my.assign(".current.coco", new.object, frame = 0)
    .invalid <- new.object@.invalid
    if (length(.invalid$type) > 0) {
        Warning("Not able to recover CoCo object!!!")
    }
    .specification <- new.object@.specification
    if (length(.specification$type) > 0) {
        if (.specification$type == "names") 
            enterNames(names = .specification$names, levels = .specification$levels, 
                missing = .specification$missing, setslot = FALSE, 
                object = new.object)
        if (.specification$type == "import") 
            importCoCo(.specification$file.name, setslot = FALSE, 
                object = new.object)
    }
    .medio <- new.object@.medio
    if (length(.medio$type) > 0) {
        if (.medio$type == "set.read") 
            setUseVariables(hit = .medio$action, set = .medio$set, 
                setslot = FALSE, object = new.object)
        if (.medio$type == "set.datastructure") 
            .set.datastructure(.medio$code, setslot = FALSE, 
                object = new.object)
    }
    .observations <- new.object@.observations
    if (length(.observations$type) > 0) {
        if (.observations$type == "table") {
            counts <- .observations$counts
            counts[counts == 2147483644] <- -1
            enterTable(counts = counts, silent = .observations$silent, 
                setslot = FALSE, object = new.object)
        }
        if (.observations$type == "list") {
            enterList(discrete = .observations$list, accumulated = .observations$accumulated, 
                ncol = .observations$ncol, select.case.fun = .observations$select.case.fun, 
                columns = .observations$columns, silent = .observations$silent, 
                setslot = FALSE, object = new.object)
        }
        if (.observations$type == "double.list") {
            .enter.double.list(list = .observations$list, accumulated = .observations$accumulated, 
                ncol = .observations$ncol, select.case.fun = .observations$select.case.fun, 
                columns = .observations$columns, silent = .observations$silent, 
                setslot = FALSE, object = new.object)
        }
        if (.observations$type == "two.list") {
            enterTwoLists(discrete = .observations$discrete, 
                continuous = .observations$continuous, accumulated = .observations$accumulated, 
                ncol = .observations$ncol, select.case.fun = .observations$select.case.fun, 
                columns = .observations$columns, silent = .observations$silent, 
                setslot = FALSE, object = new.object)
        }
    }
    .structure <- new.object@.structure
    if (!is.list(.structure[[1]])) 
        .structure <- list(.structure)
    if (length(.structure[[1]]) > 0) {
        lapply(.structure, function(x) if (length(x[[1]]) > 0) {
            if (x$type == "em.on") 
                emOn(hit = x$action, setslot = FALSE, object = new.object)
            if (x$type == "exclude.missing") 
                excludeMissing(hit = x$action, set = x$set, setslot = FALSE, 
                  object = new.object)
            if (x$type == "ordinal") 
                setOrdinal(set = x$set, setslot = FALSE, object = new.object)
            if (x$type == "q.table") 
                enterQtable(set = x$set, table = x$table, setslot = FALSE, 
                  object = new.object)
            if (x$type == "q.list") 
                enterQlist(set = x$set, list = x$list, setslot = FALSE, 
                  object = new.object)
        })
    }
    .my.trace(".sub.recover.coco,        stop:", level = level, 
        name = name, key = -1, id = .return.reference(.current.coco), 
        object = .current.coco)
    return(.current.coco)
}
".sub.recover.coco.model" <-
function (coco.model.object, name = "", key = .return.key(coco.model.object), 
    model = .return.model.of.object(coco.model.object), level = 1, 
    pos = .GlobalEnv, new.id = NULL, new.no = NULL) 
{
    .my.trace(".sub.recover.coco.model, start:", level = level, 
        name = name, key = key, model = model, id = .return.reference(coco.model.object))
    if (length(new.id) == 0) {
        object <- .recover.search(coco.model.object, key = key, 
            level = level + 1000, pos = pos)
        new.id <- .return.reference(object)
    }
    result <- .SetSlotValue(coco.model.object, ".reference", 
        new.id)
    if (length(new.no) == 0) {
        enterModel(model, object = result)
        new.no <- returnModelNumber("last", object = result)
    }
    result <- .SetSlotValue(result, ".model.number", new.no)
    my.assign(".current.coco.model", result, frame = 0)
    .my.trace(".sub.recover.coco.model,  stop:", level = level, 
        name = name, key = key, model = .return.model.of.object(result), 
        id = .return.reference(result), number = .return.model.number(result), 
        object = result)
    return(result)
}
