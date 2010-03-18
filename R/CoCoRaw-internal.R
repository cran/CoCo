# ".packageName" <- "CoCoRaw"


# ".First.lib" <-
# function (lib, pkg) 
# {
#     require(CoCoCore)
# }

# ".onAttach" <-
# function (lib, pkg) 
# {
#     require(CoCoCore)
# }

# ".onLoad" <-
# function (lib, pkg) 
# {
#     require(CoCoCore)
# }


".base" <-
function (data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    coco.simple.command(130, FALSE, object = object)
}
".clean.up" <-
function (code = "all", set = "", data = NULL, 
    object = .object.of.thing(data = data, ...), ...) 
{
    ".dispose.of.tests" <- function(object = .currentCoCo()) {
        coco.simple.command(171, FALSE, object = object)
    }
    ".dispose.of.tables" <- function(object = .currentCoCo()) {
        coco.simple.command(172, FALSE, object = object)
    }
    ".dispose.of.probabilities" <- function(object = .currentCoCo()) {
        coco.simple.command(174, FALSE, object = object)
    }
    ".dispose.of.all.q.tables" <- function(object = .currentCoCo()) {
        coco.simple.command(175, FALSE, object = object)
    }
    if ((code == "all") | (code == "tests")) 
        .dispose.of.tests(object = object)
    if ((code == "all") | (code == "tables")) 
        .dispose.of.tables(object = object)
    if ((code == "all") | (code == "estimates")) 
        .dispose.of.probabilities(object = object)
    if ((code == "all") | (code == "q-tables")) 
        .dispose.of.all.q.tables(object = object)
    if ((code == "q-table")) 
        .dispose.of.all.q.tables(set = set, object = object)
}
".current" <-
function (data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    coco.simple.command(131, FALSE, object = object)
}
".decompose.models" <-
function (set = "", model.1 = "current", model.2 = "base", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, 
        ...), ...) 
{
    coco.string.double(158, set, FALSE, model.1, model.2, 
        type = "unconditioned", object = object)
}
".dispose.of.fitted.values" <-
function (model = FALSE, data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    coco.simple.model(136, FALSE, model, object = object)
}
".eh.enter.base.model" <-
function (model = "what", data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    coco.enter.string(207, model, FALSE, object = object)
}
".enter.double.list" <-
function (list, accumulated = FALSE, ncol = FALSE, select.case.fun = FALSE, 
    columns = FALSE, silent = FALSE, setslot = TRUE, data = NULL, 
    object = .object.of.thing(data = data, ...), ...) 
{
    .set.coco.value(object, ".observations", list(type = "double.list", 
        list = list, accumulated = accumulated, ncol = ncol, 
        select.case.fun = select.case.fun, columns = columns, 
        silent = silent))
    double.list <- .split.data(list)
    if (FALSE) 
        enterList(list, accumulated, ncol, select.case.fun, 
            columns, silent, object)
    else {
        x <- length(.return.level.list(full = TRUE, object = object)) + 
            ifelse(accumulated, 1, 0)
        arg.long <- 0
        if (length(double.list$discrete) > 0) 
            arg.long <- c(t(double.list$discrete))
        arg.double <- 0
        if (length(double.list$continuous) > 0) {
            arg.double <- c(t(double.list$continuous), 0)
            arg.double[is.na(arg.double)] <- my.not.a.number()
	}
        result <- call.coco(105, sub.code = ifelse(accumulated, 2, 1), 
            arg.char = "", arg.long = arg.long, arg.double = arg.double, 
            object = object, char.ok = FALSE)
        if (any(is.na(arg.long)) || any(is.na(arg.double))) 
            excludeMissing("on", object = object)
        if (ok.coco(result)) 
            TRUE
        else NULL
    }
}
".enter.n.interactions" <-
function (order = 1, set = "*", model = FALSE, data = NULL, 
          object = .object.of.model(model, data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    if ((is.character(set) & is.number(order))) 
        result <- call.coco.message(128, FALSE, arg.char = set, 
            arg.long = order, object = object)
    .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
}
".fix.edges" <-
function (edges = "", and.fix.edges = FALSE, data = NULL, 
          object = .object.of.thing(data = data, ...), ...) 
{
    if (is.logical(edges)) {
        if (!edges) 
            coco.enter.string(ifelse(and.fix.edges, 192, 191), 
                "", FALSE, object = object)
    }
    else coco.enter.string(ifelse(and.fix.edges, 192, 191), edges, 
        FALSE, object = object)
}
".names.from.model" <-
function (model) 
{
    if (any(substring(model, 1:nchar(model), 1:nchar(model)) == 
        ",")) 
        names <- sort(unique(unlist(.split.name.set(model))))
    else names <- sort(unique(unlist(.split.model.gc(model, 
                       split.generators = TRUE))))
    nv <- c(",", ".", ";", "^")
    exclude <- match(nv, names)
    exclude <- exclude[!is.na(exclude)]
    if (length(exclude) > 0) 
        names <- names[-exclude]
    for (i in 1:length(names)) {
        name <- names[i]
        name <- substring(name, 1:nchar(name), 1:nchar(name))
        j <- match(name, nv)
        if (any(!is.na(j))) {
            name <- name[is.na(j)]
            new.name <- paste(name, collapse = "")
            names[i] <- new.name
        }
    }
    names <- sort(unique(names))
    return(names)
}
".omit.test" <-
function (data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    coco.simple.command(199, 1, object = object)
}
".plotCoCo" <-
function (x = "observed", y = "expected", set = "*", X.model = FALSE, 
    X.random = FALSE, X.log.transformed = FALSE, Y.model = FALSE, 
    Y.random = FALSE, Y.log.transformed = FALSE, complete = FALSE, 
    data = NULL, object = .object.of.models(X.model, Y.model, 
        data = data, ...), ...) 
{
    X.model <- .recover.model(X.model)
    Y.model <- .recover.model(Y.model)
    old <- .before.set.both(X.model, Y.model, object = object)
    if (!.is.nil.model(X.model) && .is.nil.model(Y.model)) 
        makeBase(old$current, object = object)
    result <- call.coco(116, 1, arg.char = set, 
                        arg.long = c(sum(c(.table.value(x), 
        ifelse(complete, 16, 0), ifelse(X.random, 32, 0))), -2, 
        ifelse(X.random, 1, 0), ifelse(X.log.transformed, 1, 
            0), sum(c(.table.value(y), ifelse(complete, 16, 0), 
            ifelse(Y.random, 32, 0))), -1, ifelse(Y.random, 1, 
            0), ifelse(Y.log.transformed, 1, 0), ifelse(complete, 
            1, 0)), object = object)
    if (ifelse(.encode.model(Y.model) < 0, FALSE, is.character(Y.model))) 
        disposeOfModel("base", object = object)
    if (!.is.nil.model(old$base)) 
        makeBase(old$base, object = object)
    .after.set.current(old$current, result, type = "long.true", 
        model = ifelse(.encode.model(X.model) < 0, FALSE, X.model), 
        object = object)
}
".return.complex" <-
function (model = FALSE, type = "expression", omit.prime.components = FALSE, 
    omit.separators = FALSE, omit.generators = FALSE, state.space = FALSE, 
    return.flags = FALSE, split.sets = FALSE, split.models = TRUE, 
    split.generators = TRUE, eliminate.empty = TRUE, data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    ".split.junction.tree" <- function(tree, split.sets = FALSE, 
        split.models = FALSE, split.generators = FALSE, 
        eliminate.empty = TRUE) {
        ".split.set.and.flags" <- function(arg, split.sets = FALSE, 
            eliminate.empty = TRUE) {
            n <- nchar(arg)
            if (n > 0) {
                chars <- substring(arg, 1:n, 1:n)
                if (any(chars == "/")) {
                  k <- (1:n)[chars == "/"]
                  set <- substring(arg, 1, k[1] - 1)
                  if (split.sets) 
                    set <- .split.name.set(set)
                  flags <- substring(arg, k[1] + 1, n)
                  return(list(set = set, flags = flags))
                }
                else return(arg)
            }
            else return(NULL)
        }
        n <- nchar(tree)
        if (n > 0) {
            chars <- substring(tree, 1:n, 1:n)
            if (any(chars == "{")) {
                k1 <- (1:n)[chars == "{"]
                k2 <- (1:n)[chars == "}"]
                tree <- substring(tree, 2, n - 1)
                n <- nchar(tree)
                chars <- substring(tree, 1:n, 1:n)
                if (any(chars == "<")) {
                  k1 <- (1:n)[chars == "<"]
                  k2 <- (1:n)[chars == ">"]
                  left <- substring(tree, 1, k1[1] - 1)
                  separator <- substring(tree, k1[1] + 0, k2[1] - 
                    0)
                  if (split.sets) 
                    separator <- .split.name.set(separator)
                  right <- substring(tree, k2[1] + 1, n)
                  return(list(left = .split.junction.tree(left, 
                    split.sets = split.sets, split.models = split.models, 
                    split.generators = split.generators, 
                    eliminate.empty = eliminate.empty), 
                    separator = separator, right = .split.junction.tree(right, 
                      split.sets = split.sets, split.models = split.models, 
                      split.generators = split.generators, 
                      eliminate.empty = eliminate.empty)))
                }
                else {
                  if (any(chars == ",")) {
                    k <- (1:n)[chars == ","]
                    set <- substring(tree, 1, k[1] - 1)
                    model <- substring(tree, k[1] + 1, n)
                    if (split.models) 
                      model <- .split.model.gc(model, 
                                         split.generators = split.generators)
                    return(list(variables = .split.set.and.flags(set, 
                      split.sets = split.sets), model = model))
                  }
                  else {
                    if (chars[2] == "[") 
                      if (split.models) 
                        return(model = .split.model.gc(tree, 
                          split.generators = split.generators))
                      else return(tree)
                    else return(.split.set.and.flags(tree, 
                                                    split.sets = split.sets))
                  }
                }
            }
            else return(tree)
        }
        else return(NULL)
    }
    old.current <- .before.set.current(model, object = object)
    result <- NULL
    .type <- .return.type(object = object)
    sub.code <- -2
    sub.code <- sub.code + ifelse(state.space, -4, 0)
    sub.code <- sub.code + ifelse(omit.generators, -8, 0)
    sub.code <- sub.code + ifelse(omit.separators, -16, 0)
    sub.code <- sub.code + ifelse(omit.prime.components, -32, 
        0)
    sub.code <- sub.code + ifelse(return.flags, -128, 0)
    if (type == "prime.components") 
        sub.code <- -2 - 8 - 16
    if (type == "separators") 
        sub.code <- -2 - 8 - 32
    if (type == "junction.tree.components") {
        if (.type != 2) 
            warning("Only implemented in mixed version of CoCo")
    }
    if (type == "reduced.expression") {
        sub.code <- sub.code - 64
        if (.type != 2) 
            warning("Only implemented in discrete version of CoCo")
    }
    result <- coco.enter.all(134, string = "what", long = NULL, 
        double = NULL, sub.code = sub.code, object = object)
    result <- .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
    if ((type == "junction.tree.components") && (.type == 2)) {
        return(.split.junction.tree(result[[1]], split.sets = split.sets, 
            split.models = split.models, split.generators = split.generators, 
            eliminate.empty = eliminate.empty))
    }
    else return(result)
}
".set.datastructure" <-
function (code = "all", setslot = TRUE, data = NULL, 
          object = .object.of.thing(data = data, 
    ...), ...) 
{
    if (("what" != code) && setslot) 
        .set.coco.value(object, ".medio", append = TRUE, 
            list(type = "set.datastructure", 
            code = code))
    x <- c("all", "necessary", "file", "large")
    x[call.coco.simple(93, .encode(c("what", x), code, c(-1, 
        1:4), 1), object = object)]
}
".set.ic" <-
function (code = "aic", kappa = 2, object = .currentCoCo()) 
{
    if (is.number(code)) 
        call.coco.reals(72, code, 1, FALSE, object = object)
    else if ((code == "kappa")) 
        if ("what" == kappa) 
            call.coco.reals(72, kappa, 1, FALSE, object = object)
        else call.coco.reals(72, kappa, 1, 4, object = object)
    else call.coco.reals(72, c(kappa), 1, .encode(c("what", "aic", 
        "bic", "off", "on"), code, c(-1, 1, 2, 3, 5), 1), object = object)
}
".set.switch" <-
function (number, hit = "flop", object = .currentCoCo()) 
{
    ".on.off" <- function(hit = "flop") {
        if (is.character(hit)) {
            x <- c("what", "off", "flop", "on")
            y <- c(-1, 1, 2, 3)
            .encode(x, hit, y, 2)
        }
        else ifelse(hit, 3, 1)
    }
    if ((number == "bic") && (hit == "on")) 
        .set.switch("ic", "on")
    x <- c("exact.test.joined.test", "exact.test.components", 
        "exact.test.unparted.test", "exact.test.only.for.log.l", 
        "partitioning", "keyboard", "echo", "diary", "timer", 
        "graph.mode", "decomposable.mode", "large", "short.test.output", 
        "report", "reuse.tests", "adjusted.df", "trace", "exact.test", 
        "exact.only.log.l", "fast", "exact.test.total", "exact.test.parts", 
        "exact.test.unparted", "graphical.search", "note", "debug", 
        "option", "log", "dump", "sorted", "keep.diary", "keep.report", 
        "keep.log", "log.data", "keep.dump", "pausing.of.output", 
        "huge", "ic", "bic", "em", "warnings")
    y <- c(17, 18, 19, 15, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
        12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
        26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)
    result <- call.coco(39, .on.off(hit), arg.long = .encode(x, 
        number, y, ifelse(is.number(number), number, 99)), object = object)
    if (ok.coco(result)) 
        ifelse((.on.off(hit) == -1), result$arg.long == 1, TRUE)
}
".show.log.lik" <-
function (model.1 = "current", model.2 = "base", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, 
        ...), ...) 
{
    coco.simple.double(161, FALSE, model.1, model.2, type = "unconditioned", 
        object = object)
}
".split.model.gc" <-
function (gc, split.generators = FALSE, eliminate.empty = TRUE, 
    structure = FALSE, d = 0) 
{
    result <- NULL
    n <- nchar(gc) - d
    if (n > 0) {
        chars <- substring(gc, 1:n, 1:n)
        if (any(chars == "|")) {
            k <- (1:n)[chars == "|"]
            if (length(k) > 0) {
                model <- substring(gc, 1, k[1] - 1)
                model <- .split.model.gc(model, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty)
                structure <- substring(gc, k[1] + 1, k[2] - 1)
                structure <- .split.model.gc(structure, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty, structure = TRUE)
                if (structure) 
                  result <- list(car = model, cdr = structure)
                else result <- list(model = model, structure = structure)
            }
        }
        else if (any(chars == "/")) {
            k <- (1:n)[chars == "/"]
            if (length(k) == 1) {
                model <- substring(gc, 1, k[1] - 1)
                options <- substring(gc, k[1] + 1, n)
                model <- .split.model.gc(model, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty)
                options <- .split.model.gc(options, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty)
                result <- list(model = model, options = options)
            }
            else {
                discrete <- substring(gc, 1, k[1] - 1)
                linear <- substring(gc, k[1] + 1, k[2] - 1)
                if (length(k) == 2) 
                  quadratic <- substring(gc, k[2] + 1, n)
                else {
                  quadratic <- substring(gc, k[2] + 1, k[3] - 
                    1)
                  rest <- substring(gc, k[3] + 1, n)
                  rest <- .split.model.gc(rest, 
                  split.generators = split.generators, 
                    eliminate.empty = eliminate.empty)
                }
                discrete <- .split.model.gc(discrete, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty)
                linear <- .split.model.gc(linear, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty)
                quadratic <- .split.model.gc(quadratic, 
                  split.generators = split.generators, 
                  eliminate.empty = eliminate.empty)
                if (length(k) == 2) 
                  result <- list(discrete = discrete, linear = linear, 
                    quadratic = quadratic)
                else result <- list(discrete = discrete, linear = linear, 
                  quadratic = quadratic, rest = rest)
            }
        }
        else if (any(chars == "[")) {
            a <- (1:n)[chars == "["]
            b <- (1:n)[chars == "]"]
            if (length(a) > 2) 
                if ((a[1] == a[2] - 1) && (a[2] == a[3] - 1)) {
                  a <- a[-1]
                  b <- b[-length(b)]
                }
            clean <- FALSE
            if (length(a) > 1) {
                if (a[1] == a[2] - 1) 
                  clean <- TRUE
                if (clean && (b[1] <= a[2])) 
                  warning("Unexpected set of parentheses.")
            }
            if (clean) 
                result <- substring(gc, a[-1] + 1, b[-length(b)] - 
                  1)
            else result <- substring(gc, a + 1, b - 1)
            if (eliminate.empty) 
                result <- result[result != ""]
            if (split.generators) 
                result <- sapply(result, function(i) .sub.split.names(i), 
                  simplify = FALSE)
        }
        else return(gc)
    }
    return(result)
}
".split.name.set" <-
function (names) 
{
    n <- nchar(names)
    if (n > 0) {
        chars <- substring(names, 1:n, 1:n)
        if (any(chars == "[")) {
            a <- (1:n)[chars == "["]
            b <- (1:n)[chars == "]"]
            if ((length(a) > 0) && (length(b) > 0)) 
                i <- substring(names, a[1] + 1, b[length(b)] - 
                  1)
            else i <- names
            return(.sub.split.names(i))
        }
        else return(.sub.split.names(names))
    }
    else return(NULL)
}
".sub.split.names" <-
function (i) 
{
    m <- nchar(i)
    if (m > 0) {
        j <- substring(i, 1:m, 1:m)
        k <- (1:length(j))[j == ":"]
        if (length(k) > 0) {
            if (!(substr(i, 1, 1) == ":")) {
                warning("Missing leading ':'")
                i <- paste(":", i, sep = "")
                m <- nchar(i)
                j <- substring(i, 1:m, 1:m)
                k <- (1:length(j))[j == ":"]
            }
            k <- (1:length(j))[j == ":"]
            return(substring(i, k, c(k[-1], m + 1) - 1))
        }
        else return(j)
    }
    else return(NULL)
}
