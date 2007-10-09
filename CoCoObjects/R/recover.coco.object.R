"recover.coco.object" <-
function (coco.object, key = .return.key(coco.object), level = 10000, 
    pos = .GlobalEnv) 
{
    ".sub.recover.coco.object" <- function(object, name = "", 
        level = 1, n = P$size["n"], p = P$size["p"], q = P$size["q"], 
        r = P$size["r"], s = P$size["s"], ss = P$size["ss"], 
        t = P$size["t"], title = object@.title, type = object@.type, 
        location = P$location, manager = P$manager, silent = FALSE, 
        sh.lib.name = P$sh.lib.name, P = object@.parameters) {
        .my.trace(".sub.recover.coco.object,start:", level = level, 
            name = name, key = -1, id = .return.reference(object))
        id.env <- .find.env(object@.key)
        if (!is.null(id.env)) {
            message("Environment already exists in .sub.recover.coco.object!")
            env <- .get.env.CoCoOBJECT(id = id.env)
            identification <- get("reference", env$env)
        }
        else {
            coco.init(n = n, p = p, q = q, r = r, s = s, ss = ss, 
                t = t, init = TRUE, title = title, type = type, 
                silent = FALSE, location = location, manager = manager, 
                sh.lib.name = sh.lib.name)
            identification <- .return.reference(.current.coco)
            env <- .CoCo.toplevel(parent = .CoCo.Root, key = object@.key, 
                reference = identification)
            id.env <- env$ID
        }
        new.object <- .SetSlotValue(object, ".reference", identification)
        new.object <- .SetSlotValue(new.object, ".id.env", id.env)
        my.assign(".current.coco", new.object, frame = 0)
        .invalid <- new.object@.invalid
        if (length(.invalid$type) > 0) {
            warning("Not able to recover CoCo object!!!")
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
                .enter.double.list(list = .observations$list, 
                  accumulated = .observations$accumulated, ncol = .observations$ncol, 
                  select.case.fun = .observations$select.case.fun, 
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
            lapply(.structure, function(x) if (length(x[[1]]) > 
                0) {
                if (x$type == "em.on") 
                  emOn(hit = x$action, setslot = FALSE, object = new.object)
                if (x$type == "exclude.missing") 
                  excludeMissing(hit = x$action, set = x$set, 
                    setslot = FALSE, object = new.object)
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
        .my.trace(".sub.recover.coco.object, stop:", level = level, 
            name = name, key = -1, id = .return.reference(.current.coco), 
            object = .current.coco)
        return(.current.coco)
    }
    .my.trace("recover.coco.object,     start:", level = level, 
        name = "bbb", key = key, id = .return.reference(coco.object))
    if ((class(coco.object) == "numeric") && (coco.object[1] == 
        .ended.coco)) 
        message("Ended CoCo object in recover.coco.object")
    if ((class(key) == "logical") && (!key)) 
        message("Ended CoCo object in recover.coco.object")
    Objects <- ls(all.names = FALSE, pos = pos)
    result <- NULL
    for (i in 1:length(Objects)) {
        .object <- get(Objects[i], pos = pos)
        if ((class(.object) == "CoCoClass")) {
            if ((.return.key(.object) == key)) {
                identification <- .return.reference(.object, 
                  test.environment = TRUE, key = key)
                if (identification == .ended.coco) {
                  cat("Recovering CoCo-object: '", Objects[i], 
                    "'.\n")
                  assign(Objects[i], .sub.recover.coco.object(object = .object, 
                    name = Objects[i], level = level + 1), pos = pos)
                }
                result <- get(Objects[i], pos = pos)
            }
        }
    }
    if (is.null(result) && (class(coco.object) == "CoCoClass")) 
        result <- .sub.recover.coco.object(object = coco.object, 
            name = "", level = level + 1)
    if (is.null(result)) 
        message("NULL result in recover.coco.object")
    .my.trace("recover.coco.object,      stop:", level = level, 
        name = "BBB", key = key, id = .return.reference(result), 
        number = .return.model.number(result), result)
    return(result)
}
