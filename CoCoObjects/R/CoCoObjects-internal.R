".but.last" <-
function (x, n = nchar(x), sep = ".") 
{
    p <- (1:n)[substring(x, 1:n, 1:n) == sep]
    b <- p[length(p)] - 1
    return(paste(substring(x, 1:b, 1:b), collapse = ""))
}
".clear.coco.objects" <-
function (coco.object = NULL, silent = FALSE, pos = .GlobalEnv) 
{
    clearCoCoObjects(coco.object = coco.object, silent = silent, 
        pos = pos, printWarnings = TRUE)
}
".CoCo.newenv" <-
function (ID) 
{
    Xlist <- list(ID = ID, env = evalq(new.env(), .GlobalEnv))
    evalq(num.subenv <- 0, Xlist$env)
    class(Xlist) <- "CoCo.env"
    Xlist
}

# ".CoCo.Root" <-
#     structure(list(ID = "", env = <environment>), 
#               .Names = c("ID", "env"), class = "CoCo.env")

.CoCo.Root <- .CoCo.newenv("")

".CoCo.toplevel" <-
function (parent = .CoCo.Root, key = "", reference = 0, number = 0, 
    ...) 
{
    ".CoCo.subenv" <- function(parent) {
        ID <- paste(parent$ID, evalq(num.subenv <- num.subenv + 
            1, parent$env), sep = ".")
        Xlist <- .CoCo.newenv(ID)
        assign(ID, Xlist, envir = parent$env)
        assign("parent", parent, envir = Xlist$env)
        assign("key", key, envir = Xlist$env)
        assign("reference", reference, envir = Xlist$env)
        assign("number", number, envir = Xlist$env)
        Xlist
    }
    .CoCo.ID <- function(Xlist) Xlist$ID
    w <- .CoCo.subenv(parent)
    ID <- .CoCo.ID(w)
    w
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
".find.env" <-
function (key, number = NULL, X = ls(.CoCo.Root$env, all.names = all.names), 
    all.names = TRUE) 
{
    result <- NULL
    for (x in X[X != "num.subenv"]) {
        env.x <- get(x, .CoCo.Root$env)
        if (class(env.x) == "CoCo.env") {
            Y <- ls(env.x$env, all.names = all.names)
            if (is.null(number)) {
                if (key == get("key", env.x$env)) 
                  result <- x
            }
            else for (y in Y[(Y != "num.subenv") & (Y != "parent")]) {
                env.y <- get(y, env.x$env)
                if (class(env.y) == "CoCo.env") {
                  Z <- ls(env.y$env, all.names = all.names)
                  if ((key == get("key", env.x$env)) && (number == 
                    get("number", env.y$env))) 
                    result <- y
                }
            }
        }
    }
    return(result)
}
".First.lib" <-
function (lib, pkg) 
{
}
".First.lib.CoCoObjects" <-
function (lib, pkg) 
{
    # require(methods)
}
".get.env.CoCoModelOBJECT" <-
function (id = CoCoModelOBJECT@.id.env, CoCoModelOBJECT = NULL, 
    env = .get.env.CoCoOBJECT(id = id.fm, env = .CoCo.Root$env)$env, 
    id.fm = if (is.null(CoCoOBJECT)) .but.last(id) else CoCoOBJECT@.id.env, 
    CoCoOBJECT = NULL, message = FALSE) 
{
    if (is.null(env)) {
        message("Not found CoCoModelOBJECT")
        NULL
    }
    else if (is.element(id, ls(env, all.names = TRUE))) {
        get(id, env)
    }
    else {
        message("Invalid CoCoModelOBJECT")
        NULL
    }
}
".get.env.CoCoOBJECT" <-
function (id = CoCoOBJECT@.id.env, CoCoOBJECT = NULL, env = .CoCo.Root$env, 
    message = FALSE) 
{
    if (is.null(env)) {
        if (message) 
            message("Not found CoCoOBJECT")
        NULL
    }
    else if (is.element(id, ls(env, all.names = TRUE))) {
        get(id, env)
    }
    else {
        if (message) 
            message("Invalid CoCoOBJECT")
        NULL
    }
}
".get.env.CoCoVIEWS" <-
function (id = CoCoVIEWS@.id.env, CoCoVIEWS = NULL, env = .get.env.CoCoModelOBJECT(id = id.fv, 
    env = env.fm)$env, id.fv = if (is.null(CoCoModelOBJECT)) .but.last(id) else CoCoModelOBJECT@.id.env, 
    CoCoModelOBJECT = NULL, env.fm = .get.env.CoCoOBJECT(id = id.fm, 
        env = .CoCo.Root$env)$env, id.fm = if (is.null(CoCoOBJECT)) .but.last(.but.last(id)) else CoCoOBJECT@.id.env, 
    CoCoOBJECT = NULL) 
{
    if (is.null(env)) {
        if (message) 
            message("Not found CoCoVIEWS")
        NULL
    }
    else if (is.element(id, ls(env, all.names = TRUE))) {
        get(id, env)
    }
    else {
        if (message) 
            message("Invalid CoCoVIEWS")
        NULL
    }
}
".is.CoCo.env" <-
function (x) 
inherits(x, "CoCo.env")
".Last.lib" <-
function (lib, pkg) 
{
}
".new.coco" <-
function (object = .current.coco, type = 1, uniq.title = FALSE, 
    title = "A CoCo object", parent = .CoCo.Root, env = .CoCo.toplevel(parent)) 
{
    result <- object
    if (.is.nil.model(title)) 
        title <- "A CoCo object"
    identification <- .return.reference(object = object)
    if (uniq.title) 
        key <- paste(title, " / ", date())
    else key <- paste(date(), identification + runif(1))
    assign("key", key, envir = env$env)
    assign("reference", identification, envir = env$env)
    result <- new("CoCoClass", .reference = identification, .id.env = env$ID, 
        .key = key, .type = type, .parameters = list(type = NULL), 
        .invalid = list(type = NULL), .specification = list(type = NULL), 
        .medio = list(type = NULL), .observations = list(type = NULL), 
        .structure = list(type = NULL), .title = title)
    my.assign(".instances.coco", c(.instances.coco, result@.reference), 
        frame = 0)
    return(result)
}
".new.coco.model" <-
function (number, model, object = .current.coco, title = "", 
    key = .return.key(object = object), identification = .return.reference(object = object), 
    id.env = .return.id.env(object), parent.id.env = if (class(object) == 
        "CoCoModelClass") .but.last(id.env) else id.env, parent = .get.env.CoCoOBJECT(id = parent.id.env), 
    env = .CoCo.toplevel(parent, key = key, reference = identification, 
        number = number)) 
{
    result <- number
    result <- new("CoCoModelClass", .reference = .return.reference(object = object), 
        .id.env = env$ID, .key = key, .type = .return.type(object = object), 
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
    .my.trace(".object.of.model,         stop:", level = 20000, 
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
    .my.trace(".object.of.thing,         stop:", level = 20000, 
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
}
".onUnload" <-
function (lib, pkg) 
{
}
".packageName" <-
"CoCoObjects"
".recover" <-
function (object = .current.coco, recover = FALSE) 
{
    key <- .return.key(object = object)
    if ((class(key) == "logical") && (!key)) 
        message("Ended CoCo object in .recover")
    identification <- .return.reference(object = object, test.environment = TRUE, 
        key = key)
    if ((!is.numeric(identification)) && (identification == FALSE)) 
        stop("Invalid memory reference (identification) of CoCo object")
    if ((identification == .ended.coco) && recover) {
        result <- .recover.coco(object = object)
        if (is.null(result)) 
            message("NULL object in .recover")
        identification <- .return.reference(result)
    }
    else {
        result <- object
    }
    if (is.null(result) || (identification == .ended.coco)) 
        stop("Recovering ended CoCo object!!!")
    if (identification == 0 || any(.coco.identifications[, 1] == 
        identification)) 
        return(result)
    else stop("Not a valid CoCoObject")
}
".recover.coco" <-
function (object, key = .return.key(object), level = 10000, pos = .GlobalEnv) 
{
    .my.trace(".recover.coco,           start:", level = level, 
        name = "aaa", key = key, id = .return.reference(object))
    new.object <- recover.coco.object(object, key = key, level = level, 
        pos = pos)
    if (is.null(new.object)) 
        message("NULL object in .recover.coco")
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
        if (is.null(result)) 
            message("NULL object in .recover.reference")
        id <- .return.reference(result)
    }
    return(id)
}
".return.id.env" <-
function (object = .current.coco) 
{
    if (class(object) == "CoCoClass" || class(object) == "CoCoModelClass") 
        is.object <- any(slotNames(object) == ".id.env")
    else is.object <- FALSE
    if (is.object) 
        return(object@.id.env)
    else return(FALSE)
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
            object <- .recover.model(object)
        return(object@.model.number)
    }
    else return(FALSE)
}
".return.reference" <-
function (object = .current.coco, test.environment = FALSE, key = .return.key(object = object)) 
{
    identification <- .sub.return.reference(object = object)
    if ((class(object) == "numeric") && (object[1] == .ended.coco)) { 
        message("Ended CoCo object in .return.reference")
        message("Problem: missing argument 'object = ...' in calling function!")
     }
    if (!is.numeric(object) && !(identification == .ended.coco) && 
        test.environment) {
        id.env <- .return.id.env(object = object)
        # message(paste(".return.reference: Identification = ", 
        #     identification, "Environment = ", id.env))
        env <- NULL
        if ((class(id.env) == "logical") && !id.env) 
            message("Old CoCo object")
        else if (class(object) == "CoCoModelClass") 
            env <- .get.env.CoCoModelOBJECT(id = id.env)
        else env <- .get.env.CoCoOBJECT(id = id.env)
        # message(paste("Number:", env$env$number, "Key:", env$env$key))
        if (is.null(env) || !(key == env$env$key)) {
            message("Hmmm ... seems the object has to be 'recovered' ... ")
            identification <- .ended.coco
        }
    }
    return(identification)
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
".sub.return.reference" <-
function (object = .current.coco) 
{
    if (is.null(object)) 
        message("NULL object in .return.reference")
    if (class(object) == "CoCoClass" || class(object) == "CoCoModelClass") 
        is.object <- any(slotNames(object) == ".reference")
    else is.object <- FALSE
    if (is.object) 
        return(object@.reference)
    else if (all(is.number(object))) 
        return(object[1])
    else return(FALSE)
}
".visit.envs" <-
function (X = ls(.CoCo.Root$env, all.names = all.names), all.names = TRUE) 
{
    print(X)
    for (x in X[X != "num.subenv"]) {
        env.x <- get(x, .CoCo.Root$env)
        if (class(env.x) == "CoCo.env") {
            print(x)
            print(ls(env.x$env, all.names = all.names))
            Y <- ls(env.x$env, all.names = all.names)
            print(paste("Key: ", get("key", env.x$env)))
            print(paste("Reference: ", get("reference", env.x$env)))
            print(paste("Number: ", get("number", env.x$env)))
            print(paste("Num.subenv: ", get("num.subenv", env.x$env)))
            for (y in Y[(Y != "num.subenv") & (Y != "parent")]) {
                env.y <- get(y, env.x$env)
                if (class(env.y) == "CoCo.env") {
                  print(y)
                  print(ls(env.y$env, all.names = all.names))
                  Z <- ls(env.y$env, all.names = all.names)
                  print(paste("  Key: ", get("key", env.y$env)))
                  print(paste("  Reference: ", get("reference", 
                    env.y$env)))
                  print(paste("  Number: ", get("number", env.y$env)))
                  print(paste("  Num.subenv: ", get("num.subenv", 
                    env.y$env)))
                  for (z in Z[(Z != "num.subenv") & (Z != "parent")]) {
                    env.z <- get(z, env.y$env)
                    if (class(env.z) == "CoCo.env") {
                      print(z)
                      print(ls(env.z$env, all.names = all.names))
                    }
                  }
                }
            }
        }
    }
}
