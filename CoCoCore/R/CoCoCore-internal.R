".after.set.current" <-
function (old.current = FALSE, result, type = "unconditioned", 
    model = FALSE, push.pop = FALSE, object = .current.coco) 
{
    ".replace.my.not.a.number" <- function(type) type
    if (is.gc(model)) 
        CoCoRaw::disposeOfModel("current", push.pop = push.pop, 
            object = object)
    if (length(old.current) > 0) 
        if (old.current) 
            CoCoRaw::makeCurrent(old.current, object = object)
    if ((type == "unconditioned") || (length(result) == 0)) 
        result
    else if (type == "ok") 
        CoCoCore::ok.coco(result)
    else if (type == "long.true") 
        unlist(ifelse(ok.coco(result), result$arg.long[1] == 1, list(NULL)))
    else if (type == "double") {
        if (ok.coco(result)) 
            .replace.my.not.a.number(result$arg.double)
    }
    else if (type == "long.and.double") 
        if (ok.coco(result)) 
            c(result$arg.long, .replace.my.not.a.number(result$arg.double))
}
".before.set.both" <-
function (model.1 = "current", model.2 = "base", push.pop = FALSE, 
    object = .current.coco) 
{
    if (.is.nil.model(model.1))
        model.1 <- "current"
    if (.is.nil.model(model.2))
        model.2 <- "base"
    model.1 <- CoCoObjects::.recover.model(model.1)
    if (class(model.1) == "CoCoModelClass") 
        model.1 <- CoCoRaw::returnModelNumber(model.1, object = object)
    model.2 <- CoCoObjects::.recover.model(model.2)
    if (class(model.2) == "CoCoModelClass") 
        model.2 <- CoCoRaw::returnModelNumber(model.2, object = object)
    if (push.pop) 
        old.current <- FALSE
    else old.current <- CoCoRaw::returnModelNumber("current", 
        no.warnings = TRUE, object = object)
    if (is.null(old.current))
	old.current <- FALSE
    if (push.pop) 
        old.base <- FALSE
    else old.base <- CoCoRaw::returnModelNumber("base", 
         no.warnings = TRUE, object = object)
    if (is.null(old.base))
	old.base <- FALSE
    identical <- FALSE
    if (is.numeric(model.2)) {
        type <- .return.type(object)
        if (type == 2) 
            identical <- (class(model.1) == class(model.2)) &&
                          (model.1 == model.2)
        makeBase(model.2, both = identical, push = push.pop, 
            object = object)
    }
    else {
        CoCoCore::.set.current.model(model.2, old.current, old.base, 
                                     object = object)
        CoCoRaw::.base(object = object)
     }
    if (!identical) {
        if ((old.current != FALSE) && 
            (is.character(model.1) && (model.1 == "current")))
            CoCoRaw::makeCurrent(old.current, object = object)
        CoCoCore::.set.current.model(model.1, old.current, old.base, 
            object = object)
    }
    return(list(base = old.base, current = old.current))
}
".before.set.current" <-
function (model = FALSE, push.pop = FALSE, object = .current.coco) 
{
    model <- CoCoObjects::.recover.model(model)
    if (!CoCoCore::.is.nil.model(model) && !.is.current.model(model)) {
        if (push.pop) 
            old.current <- FALSE
        else old.current <- CoCoRaw::returnModelNumber("current", 
            no.warnings = TRUE, object = object)
        if (is.null(old.current))
	    old.current <- FALSE
        x <- CoCoObjects::.return.object.model.number(model, 
            recover = FALSE, object = object)
        if (CoCoCore::.is.nil.model(x)) 
            x <- model
        CoCoCore::.set.current.model(x, old.current, old.base = FALSE, 
            push.pop = push.pop, object = object)
        return(old.current)
    }
    else return(FALSE)
}
".coco.command.implemented" <-
function (code, sub.code = 0, arg.char = "", arg.long = NULL, 
    arg.double = NULL, arg.char.int = NULL, object = .current.coco) 
{
    if ((-3 == code)) 
        stop("Do not enter CoCo in R+CoCo under Windows.")
    if ((93 == code) && (2 <= sub.code)) 
        warning("File as datastructure not implemented in R+CoCo under Windows (LevelFile)")
    if ((93 == code) && (5 == sub.code)) 
        warning("HUGE not implemented in R+CoCo under Windows (IntegerFile)")
    if ((115 == code) && (5 == sub.code)) 
        warning("No sorted list for large tables (RealFile)")
    if ((1 <= code) && (code <= 1)) 
        warning("Restart not avaliable in R+CoCo under Windows")
    if ((2 <= code) && (code <= 10)) 
        warning("Modification of parser not to be implemented in R+CoCo under Windows (?)")
    if ((26 <= code) && (code <= 27)) 
        warning("No redirection of standard output in R+CoCo under Windows")
    return(TRUE)
}
".coco.id" <-
function (object = .current.coco, recover = FALSE) 
{
    id <- .return.reference(object = object)
    if ((!is.numeric(id)) & (id == FALSE)) 
        stop("Invalid memory reference (identification) of CoCo object")
    if ((id == .ended.coco) && recover) 
        id <- .recover.reference(coco.object = object)
    if (id == .ended.coco) 
        stop("Ended CoCo object")
    if (id == 0 || any(.coco.identifications[, 1] == id)) 
        return(id)
    else stop("Not a valid CoCoObject")
}
".coco.load" <-
function (sh.lib.name = NULL) 
{
}
".empty.string" <-
function (n) 
paste(rep("", n), collapse = "")
".encode" <-
function (x, a, y = 1:length(x), d = FALSE) 
{
    if (length(x) != length(y)) 
        stop("Encoding error")
    b <- y[x == a[1]]
    if (length(b) == 1) 
        b
    else ifelse(d != FALSE, d, a[1])
}
".encode.model" <-
function (number, current.default = FALSE) 
{
    if (class(number) == "CoCoModelClass") 
        number@.model.number
    else
      .encode(c("base", "current", "last", "previous", "next"), 
              number, -(1:5), 
              ifelse(current.default, -2, 
                     ifelse(.is.nil.model(number), -2, number)))
}
".encode.model.1" <-
function (number, object = .current.coco) 
{
    x <- .return.object.model.number(number, recover = TRUE, 
        object = object)
    if (x) 
        x
    else if (is.null(number)) 
        -2
    else if (is.number(number)) 
        number
    else {
        x <- .encode(c("base", "current", "last", "previous", "next"),
                     number, -(1:5)
#                     , ifelse(FALSE, -2, 
#                            ifelse(.is.nil.model(number), -2, number))
                     )
        if (x < 0) 
            x
        else returnModelNumber(number, object = object)
    }
}
".encode.type.and.options" <-
function (type, random = FALSE, log.transformed = FALSE, complete = FALSE, 
    permuted = TRUE, uniform = FALSE, rankit = FALSE, probit = FALSE) 
{
    c(.table.value(type), -2, ifelse(random, 1, 0), ifelse(log.transformed, 
        1, 0), ifelse(complete, 1, 0), ifelse(permuted, 1, 0), 
        ifelse(uniform, 1, 0), ifelse(rankit, 1, 0), ifelse(probit, 
            1, 0))
}
".encode.visit" <-
function (action) 
{
    .encode(c("show", "print", "describe", "dispose", "fit", 
        "accept", "reject"), action, c(137, 137:139, 221:223), 
        137)
}
".false.if.NULL" <-
function (arg) 
ifelse(length(arg) == 0, FALSE, arg)
".First.lib" <-
function (lib, pkg) 
{
    # require(methods)
    # require(CoCoObjects)
    library.dynam("CoCoCore", pkg, lib)
    .First.lib.CoCoCore(lib, pkg)
}
".First.lib.CoCoCore" <-
function (lib, pkg) 
{
    my.assign(".char.ok", FALSE, frame = 0)
    coco.load()
#   cat("For documentation see, Badsberg, J.H.: A guide to CoCo,                \n")
#   cat("JSS, 2001 ( http://www.jstatsoft.org/v06/i04/ )                        \n")
#   cat("and Badsberg, J.H.: Xlisp+CoCo, Aalborg, 1996.                         \n")
#   cat("The manual pages of CoCo in R will currently only give you             \n")
#   cat("the form of arguments of the functions. The teen calls                 \n")
#   cat("''                                                                     \n")
#   cat("      data(Reinis);                                                    \n")
#   cat("      CoCoObject <- makeCoCo();                                        \n")
#   cat("      enterTable(Reinis, object = CoCoObject);                         \n")
#   cat("      fullModel <- makeModel(enterModel(\"*\", object = CoCoObject));    \n")
#   cat("      library(CoCoGraph);                                              \n")
#   cat("      fullGraph <- dynamic.Graph(fullModel, title = \"Full\");           \n")
#   cat("      backward(recursive = TRUE, object = CoCoObject);                 \n")
#   cat("      eh(object = CoCoObject);                                         \n")
#   cat("      lastModel <- makeModel(\"last\", object = CoCoObject);             \n")
#   cat("      backwardGraph <- dynamic.Graph(lastModel, title = \"Last\");       \n")
#   cat("''                                                                     \n")
#   cat("will give you a small example.                                         \n")
#   cat("Please quit by 'quitCoCo()' to remove temporary files.                 \n")
#   cat("\n")
    .clear.coco.objects(pos = .GlobalEnv)
}
".get.cocolib.name" <-
function (x) 
system("echo $COCOLIB", intern = TRUE)
".get.lib.and.tmp" <-
function (sub.dir = "library/CoCo/lib/coco") 
{
    coco.lib <- paste(Sys.getenv("R_HOME"), sub.dir, sep = "/")
    coco.tmp <- Sys.getenv("TEMP")
    if (coco.tmp == "") 
        coco.tmp <- Sys.getenv("PWD")
    Arg.char <- paste("@", coco.lib, "@", coco.tmp, "@", sep = "")
    return(Arg.char)
}
".get.rs.cshlib.name" <-
function (x) 
system("echo $RSCSHLIB", intern = TRUE)
".is.current.model" <-
function (model) 
{
    if (is.character(model)) 
        return(model == "current")
    else return(FALSE)
}
".is.nil.model" <-
function (model) 
{
    if (is.logical(model)) 
        return(!model)
    if (is.null(model)) 
        return(TRUE)
    else return(FALSE)
}
".Last.lib" <-
function (lib, pkg) 
{
}
".mips.load" <-
function (sh.lib.name = NULL) 
{
    library.dynam("Mips", pkg, lib)
}
".my.ascii" <-
function () 
sapply(0:127, function(i) parse(text = paste("\"\\", structure(i, 
    class = "octmode"), "\"", sep = ""))[[1]])
".my.dyn.load.open" <-
function (file) 
{
    cat("now dyn.load(", file, ") ... \n", sep = "")
    dyn.load(file)
}
".my.getenv" <-
function (X) 
system(paste("echo $", X, sep = ""), intern = TRUE)
".my.test.file.found" <-
function (Xfile) 
{
    Xtest <- paste("ls ", Xfile, sep = "")
    return(length(system(Xtest, intern = TRUE)) > 0)
}
".my.trace" <-
function (label, vector = NULL, level = 1, name = "", key = -1, 
    model = "", id = -1, number = -1, object = NULL) 
{
}
".object.of.models" <-
function (model.1, model.2, data = NULL, object = .current.coco,
    names = NULL, levels = NULL, to.factor = NULL, ...)
{
    if ((class(model.1) == "CoCoModelClass"))
      result <- .object.of.model(model.1, data = data, object = object,
                                 names = names, levels = levels,
                                 to.factor = to.factor, ...)
    else
      result <- .object.of.model(model.2, data = data, object = object,
                                 names = names, levels = levels,
                                 to.factor = to.factor, ...)
    return(result)
}
".onAttach" <-
function (lib, pkg) 
{
    require(CoCoObjects)
}
".onLoad" <-
function (lib, pkg) 
{
    require(CoCoObjects)
    library.dynam("CoCoCore", pkg, lib)
}
# ".packageName" <-
# "CoCoCore"
".return.factor.type.list" <-
function (full = FALSE, number.variates = numberVariates(full = full, 
    object = object), object = .current.coco) 
{
    force(number.variates)
    call.coco.longs(145, rep(0, number.variates), number.variates, 
        ifelse(full, 3, 6), object = object)
}
".return.level.list" <-
function (full = FALSE, number.variates = numberVariates(full = full, 
    object = object), object = .current.coco) 
{
    force(number.variates)
    call.coco.longs(145, rep(0, number.variates), number.variates, 
        ifelse(full, 1, 4), object = object)
}
".return.missing.list" <-
function (full = FALSE, number.variates = numberVariates(full = full, 
    object = object), object = .current.coco) 
{
    force(number.variates)
    call.coco.longs(145, rep(0, number.variates), number.variates, 
        ifelse(full, 2, 5), object = object)
}
".return.name.list.string" <-
function (full = FALSE, object = .current.coco) 
{
    coco.enter.string(144, "what", ifelse(full, 2, 1), object = object)
}
".set.acceptance" <-
function (alfa = 0.05, object = .current.coco) 
{
    call.coco.reals(69, alfa, 1, FALSE, object = object)
}
".set.components" <-
function (components.limit = 0.01, object = .current.coco) 
{
    call.coco.reals(73, components.limit, 1, FALSE, object = object)
}
".set.current.model" <-
function (model = FALSE, old.current = FALSE, old.base = FALSE, 
    push.pop = FALSE, object = .current.coco) 
{
    x <- FALSE
    if (is.character(model)) {
        if ((model == "base")) 
            x <- ifelse(old.base, old.base, returnModelNumber("base", 
                object = object))
        else if ((model == "current")) 
            x <- ifelse(old.current, old.current, returnModelNumber("current", 
                object = object))
        else if ((model == "last")) 
            x <- returnModelNumber("last", object = object)
        else if ((model == "next")) {
	    warning("Using current model for next")
            x <- returnModelNumber("current", object = object)
        } else if ((model == "previous")) {
	    warning("Using current model for previous")
            x <- returnModelNumber("current", object = object)
        } else if (is.gc(model)) 
            enterModel(model, object = object)
    }
    else {
        if (!.is.nil.model(model)) 
            x <- .return.object.model.number(model, recover = FALSE, 
                object = object)
    }
    if (!.is.nil.model(x)) 
        makeCurrent(x, push = push.pop, object = object)
}
".set.rejection" <-
function (alfa.rejected = 0.025, object = .current.coco) 
{
    call.coco.reals(87, alfa.rejected, 1, FALSE, object = object)
}
".set.separators" <-
function (separators.limit = 0.001, object = .current.coco) 
{
    call.coco.reals(74, separators.limit, 1, FALSE, object = object)
}
".split.data" <-
function (argument) 
{
    discrete <- NULL
    continuous <- NULL
    for (i in 1:dim(argument)[2]) if (is.factor(argument[, i])) 
        discrete <- cbind(discrete, argument[, i])
    else continuous <- cbind(continuous, argument[, i])
    return(list(discrete = discrete, continuous = continuous))
}
".table.value" <-
function (type) 
{
    x <- c("counts", "observed", "probabilities", "expected", 
        "unadjusted", "absolute", "f-res", "r-f", "g-res", "r-g", 
        "adjusted", "leverage", "c-res", "m-res", "standardized", 
        "standard", "x-res", "deviance", "-2log", "l-res", "freeman-tukey", 
        "2n-m", "sqrt", "power", "index", "zero", "error", "canonical", 
        "gs", "hs", "ks", "moment", "means", "covariance", "raw", 
        "total", "ss", "ssds", "sigma", "determinants", "mk", 
        "ms")
    y <- c(0, 0, 1, 2, 3, 3, 4, 5, 6, 7, 8, 16, 8, 9, 9, 9, 9, 
        10, 10, 10, 11, 12, 12, 13, 14, 15, 63, 16 + c(2, 3, 
            4, 5, 6, 7, 8, 9, 10, 11, 12, 12, 13, 14, 15))
    result <- .encode(x, type, y, type)
    if (is.number(result)) 
        return(result)
    else {
        warning("Invalid type")
        return(0)
    }
}
".to.search" <-
function (action, model = FALSE, a = FALSE, b = FALSE, object = .current.coco) 
{
    .visit.model(model, a, b, action = action, object = object)
}
".visit.interval" <-
function (from, to, action = c("show", "describe", "dispose", 
    "fit", "accept", "reject"), object = .current.coco) 
{
    result <- NULL
    old.current <- .before.set.current(to, object = object)
    old.base <- returnModelNumber("base", no.warnings = TRUE, 
        object = object)
    i <- returnModelNumber("current", no.warnings = TRUE, object = object)
    while (from <= i) {
        ok <- makeCurrent(i, object = object)
        if (ok) 
            result <- coco.simple.command(.encode.visit(action), 
                2, object = object)
        if ((action == "print") || (action == "show")) {
            if (!is.null(old.current) && (i == old.current))
                cat(" // Current model //\n")
            if (!is.null(old.base) && (i == old.base))
                cat(" // Base model //\n")
        }
        if (from < i) {
            ok <- makeCurrent("previous", object = object)
            if (ok) 
                i <- returnModelNumber("current", object = object)
            else j <- j - 1
        }
        else i <- i - 1
    }
    .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
}
".visit.model" <-
function (model = FALSE, a = FALSE, b = FALSE, action = c("show", 
    "describe", "dispose", "fit", "accept", "reject"), object = .current.coco) 
{
    ".dispose.of.interval" <- function(from, to, object = .current.coco) {
        result <- NULL
        old.current <- .before.set.current(to, object = object)
        i <- returnModelNumber("current", object = object)
        while (from <= i) {
            makeCurrent(i, object = object)
            if (from < i) {
                ok <- makeCurrent("previous", object = object)
                if (ok) 
                  j <- returnModelNumber("current", object = object)
                else j <- i - 1
            }
            else j <- i - 1
            ok <- makeCurrent(i, object = object)
            if (ok) 
                result <- coco.simple.command(139, 2, object = object)
            i <- j
        }
        .after.set.current(old.current, result, type = "unconditioned", 
            model = FALSE, object = object)
    }
    x <- .return.object.model.number(model, recover = TRUE, object = object)
    if ((length(x) > 1) || (!is.null(x) && x)) 
        .visit.models(x, action = action, object = object)
    else if (is.null(model))
        .visit.models("current", action = action, object = object)
    else if (is.character(model) && (length(model) > 1)) 
        .visit.models(model, action = action, object = object)
    else if ((model == "number") | (model == "list"))
        .visit.models(a, action = action, object = object)
    else if (is.number(model))
        .visit.models(model, action = action, object = object)
    else if (model == "interval") {
        if (action == "dispose") 
            .dispose.of.interval(a, b, object = object)
        else .visit.interval(a, b, action = action, object = object)
    }
    else
        coco.simple.command(.encode.visit(action), .encode(c("base", 
            "current", "last", "all"), model, 1:4, 2), object = object)
}
".visit.models" <-
function (list, action = c("show", "describe", "dispose", "fit", 
    "accept", "reject"), object = .current.coco) 
{
    result <- NULL
    if (is.character(list) && (length(list) == 1)) 
        .visit.model(list, action = action, object = object)
    else {
        old.current <- returnModelNumber("current", no.warnings = TRUE, 
            object = object)
        old.base <- returnModelNumber("base", no.warnings = TRUE, 
            object = object)
        for (i in list) {
            if (i == "current") 
                ok <- makeCurrent(old.current, object = object)
            else ok <- makeCurrent(i, object = object)
            if (ok) 
                result <- coco.simple.command(.encode.visit(action), 
                  2, object = object)
            if ((action == "print") || (action == "show")) {
                if (!is.null(old.current) && (i == old.current))
                  cat(" // Current model //\n")
                if (!is.null(old.base) && (i == old.base))
                  cat(" // Base model //\n")
            }
        }
        .after.set.current(old.current, result, type = "unconditioned", 
            model = FALSE, object = object)
    }
}
