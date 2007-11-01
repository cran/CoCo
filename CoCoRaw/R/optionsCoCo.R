"optionsCoCo" <-
function (..., section = "all", status = FALSE, force.files = FALSE, 
    data = NULL, object = .object.of.thing(data = data)) 
{
    ".option.long" <- function(result, names, names.both, names.args, 
        args, code, m = length(names), object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, names)) > 0) {
            X <- call.coco.longs(code, "what", m, FALSE, object = object)
            for (i in 1:length(names)) {
                if (is.element(names[i], names.both)) {
                  result <- append(result, list(X[i]))
                  names(result)[length(result)] <- names[i]
                }
                if (is.element(names[i], names.args)) 
                  X[i] <- args[[(1:(length(args)))[names.args == 
                    names[i]]]]
            }
            if (length(intersect(names.args, names)) > 0) 
                call.coco.longs(code, X, m, FALSE, object = object)
        }
        return(result)
    }
    ".option.longs" <- function(result, names, names.both, names.args, 
        args, code, m = 25, stop.mark = 0, object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, names)) > 0) {
            X <- call.coco.longs(code, "what", m, FALSE, object = object)
            X <- X[1:max((1:length(X))[X != stop.mark])]
            if (is.element(names, names.both)) {
                result <- append(result, list(X))
                names(result)[length(result)] <- names
            }
            if (is.element(names, names.args)) 
                X <- args[[(1:(length(args)))[names.args == names]]]
            if (length(intersect(names.args, names)) > 0) 
                call.coco.longs(code, X, m, FALSE, object = object)
        }
        return(result)
    }
    ".option.name" <- function(result, names, names.both, names.args, 
        args, code, object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, names)) > 0) {
            X <- call.coco.chars(code, "what", FALSE, object = object)
            if (is.element(names, names.both)) {
                result <- append(result, list(X))
                names(result)[length(result)] <- names
            }
            if (is.element(names, names.args)) 
                X <- args[[(1:(length(args)))[names.args == names]]]
            if (length(intersect(names.args, names)) > 0) 
                call.coco.chars(code, X, FALSE, object = object)
        }
        return(result)
    }
    ".option.real" <- function(result, names, names.both, names.args, 
        args, code, m = length(names), object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, names)) > 0) {
            X <- call.coco.reals(code, "what", m, FALSE, object = object)
            for (i in 1:length(names)) {
                if (is.element(names[i], names.both)) {
                  result <- append(result, list(X[i]))
                  names(result)[length(result)] <- names[i]
                }
                if (is.element(names[i], names.args)) 
                  X[i] <- args[[(1:(length(args)))[names.args == 
                    names[i]]]]
            }
            if (length(intersect(names.args, names)) > 0) 
                call.coco.reals(code, X, m, FALSE, object = object)
        }
        return(result)
    }
    ".option.long.and.real" <- function(result, long.names, real.names, 
        names.both, names.args, args, code, long.m = length(long.names), 
        real.m = length(real.names), object = CoCoCore::.currentCoCo(),
        sub.code = 1) {
        if (length(intersect(names.both, long.names)) + length(intersect(names.both, 
            real.names)) > 0) {
            Z <- coco.enter.all(code, "what", sub.code = -sub.code,
                                object = object)
            X <- Z[[2]]
            Y <- Z[[3]]
            for (i in 1:length(long.names)) {
                if (is.element(long.names[i], names.both)) {
                  result <- append(result, list(X[i]))
                  names(result)[length(result)] <- long.names[i]
                }
                if (is.element(long.names[i], names.args)) 
                  X[i] <- args[[(1:(length(args)))[names.args == 
                    long.names[i]]]]
            }
            for (i in 1:length(real.names)) {
                if (is.element(real.names[i], names.both)) {
                  result <- append(result, list(Y[i]))
                  names(result)[length(result)] <- real.names[i]
                }
                if (is.element(real.names[i], names.args)) 
                  Y[i] <- args[[(1:(length(args)))[names.args == 
                    real.names[i]]]]
            }
            if (length(intersect(names.args, c(long.names, real.names))) > 
                0) 
                coco.replace.all(code, string = "", long = X, 
                  double = Y, sub.code = sub.code, object = object)
        }
        return(result)
    }
    ".option.code.by.long" <- function(result, name, names.both, 
        names.args, args, code, x = c("a", "b", "c"), y = 1:3, 
        object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, name)) > 0) {
            X <- x[call.coco.longs(code, 1, 1, -1, object = object)]
            if (is.element(name, names.both)) {
                result <- append(result, list(X))
                names(result)[length(result)] <- name
            }
            if (is.element(name, names.args)) 
                X <- args[[(1:(length(args)))[names.args == name]]]
            if (length(intersect(names.args, name)) > 0) 
                call.coco.longs(code, .encode(x, X, y, 1), 1, 
                  FALSE, object = object)
        }
        return(result)
    }
    ".option.code" <- function(result, name, names.both, names.args, 
        args, code, x = c("a", "b", "c"), y = 1:3, object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, name)) > 0) {
            X <- x[call.coco.simple(code, "what", object = object)]
            if (is.element(name, names.both)) {
                result <- append(result, list(X))
                names(result)[length(result)] <- name
            }
            if (is.element(name, names.args)) 
                X <- args[[(1:(length(args)))[names.args == name]]]
            if (length(intersect(names.args, name)) > 0) 
                call.coco.simple(code, .encode(x, X, y, 1), object = object)
        }
        return(result)
    }
    ".option.switch" <- function(result, name, names.both, names.args, 
        args, switch.name = name, object = CoCoCore::.currentCoCo()) {
        if (length(intersect(names.both, name)) > 0) {
            X <- .set.switch(switch.name, "what", object = object)
            if (is.element(name, names.both)) {
                result <- append(result, list(X))
                names(result)[length(result)] <- name
            }
            if (is.element(name, names.args)) 
                X <- args[[(1:(length(args)))[names.args == name]]]
            if (length(intersect(names.args, name)) > 0) 
                .set.switch(switch.name, X, object = object)
        }
        return(result)
    }
    "options.formats" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("formats", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.page <- c("width", "height")
        names.table <- c("digits.table", "decimals.table.probabilities", 
            "decimals.table.expected", "decimals.table.residual")
        names.test <- c("digits.test.statistics", "decimals.test.statistics", 
            "digits.test.pvalues", "decimals.test.pvalues")
        names.print <- c("digits", "decimals")
        names.rest <- c("pausing.of.output", "n.lines", "short.test.output")
        if (length(names.both) == 0) {
            names.both <- c(names.page, names.table, names.test, 
                names.print, names.rest)
            names.return <- names.both
        }
        result <- NULL
        result <- .option.long(result, names.page, names.both, 
            names.args, args, 48, m = 2, object = object)
        result <- .option.long(result, names.table, names.both, 
            names.args, args, 46, m = 4, object = object)
        result <- .option.long(result, names.test, names.both, 
            names.args, args, 47, m = 4, object = object)
        result <- .option.long(result, names.print, names.both, 
            names.args, args, 45, m = 2, object = object)
        result <- .option.switch(result, names.rest[1], names.both, 
            names.args, args, object = object)
        result <- .option.long(result, names.rest[2], names.both, 
            names.args, args, 63, m = 1, object = object)
        result <- .option.switch(result, names.rest[3], names.both, 
            names.args, args, object = object)
        return(result)
    }
    "options.tests" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("tests", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.code <- c("algorithm", "search.statistic")
        names.switch <- c("partitioning", "adjusted.df", "ic", 
            "bic", "decomposable.mode", "reuse.tests")
        names.rest <- c("power.lambda", "ic.kappa")
        names.limits <- c("acceptance.limit", "rejection.limit", 
            "components.limit", "separators.limit")
        if (length(names.both) == 0) {
            names.both <- c(names.code, names.switch, names.limits, 
                names.rest)
            names.return <- names.both
        }
        result <- NULL
        result <- .option.code(result, names.code[1], names.both, 
            names.args, args, 65, x = c("a", "b", "c"), y = 1:3, 
            object = object)
        result <- .option.switch(result, names.switch[1], names.both, 
            names.args, args, object = object)
        result <- .option.switch(result, names.switch[2], names.both, 
            names.args, args, object = object)
        result <- .option.real(result, names.rest[1], names.both, 
            names.args, args, 71, m = 1, object = object)
        result <- .option.switch(result, names.switch[3], names.both, 
            names.args, args, object = object)
        result <- .option.switch(result, names.switch[4], names.both, 
            names.args, args, object = object)
        result <- .option.real(result, names.rest[2], names.both, 
            names.args, args, 72, m = 1, object = object)
        result <- .option.switch(result, names.switch[5], names.both, 
            names.args, args, object = object)
        result <- .option.code(result, names.code[2], names.both, 
            names.args, args, 70, x = c("deviance", "pearson", 
                "power", "lr", "chisq"), y = c(1:3, 1:2), object = object)
        result <- .option.real(result, names.limits[1], names.both, 
            names.args, args, 69, m = 4, object = object)
        result <- .option.real(result, names.limits[2], names.both, 
            names.args, args, 87, m = 4, object = object)
        result <- .option.real(result, names.limits[3], names.both, 
            names.args, args, 73, m = 4, object = object)
        result <- .option.real(result, names.limits[4], names.both, 
            names.args, args, 74, m = 4, object = object)
        result <- .option.switch(result, names.switch[6], names.both, 
            names.args, args, object = object)
        return(result)
    }
    "options.ips" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("ips", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.ips <- c("ips.criterion", # "cips.criterion", 
            "ips.algorithm", "ips.number.of.iterations", "ips.epsilon")
        if (length(names.both) == 0) {
            names.both <- names.ips
            names.return <- names.both
        }
        result <- NULL
        result <- .option.code(result, names.ips[1], names.both, 
            names.args, args, 53, 
            x = c("cell", "sum", "suff", "rell"), y = 1:4, 
            object = object)
        # result <- .option.code(result, names.ips[2], names.both, 
        #     names.args, args, 79, 
        #     x = c("cell", "sum", "suff", "rell"), y = 1:4, 
        #     object = object)
        result <- .option.code(result, names.ips[2], names.both, 
            names.args, args, 62, x = c("normal", "arithmetic", 
                "geometric", "harmonic"), y = 1:4, object = object)
        result <- .option.long(result, names.ips[3], names.both, 
            names.args, args, 56, m = 1, object = object)
        result <- .option.real(result, names.ips[4], names.both, 
            names.args, args, 55, m = 1, object = object)
        return(result)
    }
    "options.cips" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("ips", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.crit <- c("cips.criterion")
        names.long <- c("cips.cycles")
        names.real <- c("cips.epsilon")
        names.cips <- c(names.crit, names.long, names.real)
        if (length(names.both) == 0) {
            names.both <- names.cips
            names.return <- names.both
        }
        result <- NULL
        result <- .option.code(result, names.crit, names.both, 
            names.args, args, 79, 
            x = c("cell", "sum", "suff", "rell"), y = 1:4, 
            object = object)
        result <- .option.long.and.real(result, names.long, names.real, 
            names.both, names.args, args = args, code = 79, object = object,
            sub.code = 33)
        return(result)
    }
    "options.em" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("em", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.em <- c("em", "em.number.of.iterations", "em.epsilon", 
            "em.initial")
        if (length(names.both) == 0) {
            names.both <- names.em
            names.return <- names.both
        }
        result <- NULL
        result <- .option.switch(result, names.em[1], names.both, 
            names.args, args, object = object)
        result <- .option.long(result, names.em[2], names.both, 
            names.args, args, 59, m = 1, object = object)
        result <- .option.real(result, names.em[3], names.both, 
            names.args, args, 58, m = 1, object = object)
        result <- .option.code.by.long(result, names.em[4], names.both, 
            names.args, args, 57, x = c("uniform", "first", "last", 
                "mean", "random", "input"), y = 1:6, object = object)
        return(result)
    }
    "options.mixed" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("mips", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.mips <- c("mixed.criterion")
        names.long <- c("mixed.cycles")
        names.real <- c("mixed.epsilon", "mixed.init.epsilon", 
            "mixed.log.l.round.error", "mixed.random.noise", 
            "mixed.min.lambda", "cholesky.epsilon")
        names.mixed <- c(names.mips, names.long, names.real)
        if (length(names.both) == 0) {
            names.both <- names.mixed
            names.return <- names.both
        }
        result <- NULL
        result <- .option.code(result, names.mips[1], names.both, 
            names.args, args, 61, 
            x = c("cell", "sum", "suff", "rell"), y = 1:4, 
            object = object)
        result <- .option.long.and.real(result, names.long, names.real, 
            names.both, names.args, args = args, code = 60, object = object)
        return(result)
    }
    "options.exact" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("exact", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.exact <- c("exact.test", "exact.epsilon", "exact.test.joined.test", 
            "exact.test.components", "exact.test.unparted.test", 
            "exact.test.only.for.log.l", "exact.number.of.tables", 
            "exact.list", "exact.asymptotic", "fast", "exact.seed")
        if (length(names.both) == 0) {
            names.both <- names.exact
            names.return <- names.both
        }
        result <- NULL
        result <- .option.code(result, names.exact[1], names.both, 
            names.args, args, 75, x = c("off", "flop", "on", 
                "all", "deviance"), y = 1:5, object = object)
        result <- .option.real(result, names.exact[2], names.both, 
            names.args, args, 84, m = 1, object = object)
        result <- .option.switch(result, names.exact[3], names.both, 
            names.args, args, object = object)
        result <- .option.switch(result, names.exact[4], names.both, 
            names.args, args, object = object)
        result <- .option.switch(result, names.exact[5], names.both, 
            names.args, args, object = object)
        result <- .option.switch(result, names.exact[6], names.both, 
            names.args, args, object = object)
        result <- .option.long(result, names.exact[7], names.both, 
            names.args, args, 80, m = 1, object = object)
        result <- .option.longs(result, names.exact[8], names.both, 
            names.args, args, 85, m = 15, object = object)
        result <- .option.real(result, names.exact[9], names.both, 
            names.args, args, 76, m = 1, object = object)
        result <- .option.switch(result, names.exact[10], names.both, 
            names.args, args, object = object)
        result <- .option.long(result, names.exact[11], names.both, 
            names.args, args, 78, m = 1, object = object)
        return(result)
    }
    "options.other" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("other", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.other <- c("graph.mode", "report", "trace", "debug", 
            "timer", "echo", "note", "sorted", "large", "huge", 
            "graphical.search", "warnings")
        if (length(names.both) == 0) {
            names.both <- names.other
            names.return <- names.both
        }
        result <- NULL
        for (i in 1:length(names.other)) {
            result <- .option.switch(result, names.other[i], 
                names.both, names.args, args, object = object)
        }
        return(result)
    }
    "options.files" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("files", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.files <- c("diary", "diary.name", "keep.diary", 
            "dump", "dump.name", "keep.dump", "report", "report.name", 
            "keep.report", "log", "log.name", "keep.log", "log.data")
        if (length(names.both) == 0) {
            names.both <- names.files
            names.return <- names.both
        }
        result <- NULL
        for (i in 1:4) {
            result <- .option.switch(result, names.files[1 + 
                3 * (i - 1)], names.both, names.args, args, object = object)
            result <- .option.name(result, names.files[2 + 3 * 
                (i - 1)], names.both, names.args, args, c(25, 
                30, 28, 29)[i], object = object)
            result <- .option.switch(result, names.files[3 + 
                3 * (i - 1)], names.both, names.args, args, object = object)
        }
        result <- .option.switch(result, names.files[13], names.both, 
            names.args, args, object = object)
        return(result)
    }
    "options.datafiles" <- function(..., status = FALSE, object = CoCoCore::.currentCoCo()) {
        if (status) 
            showOptions("data", object = object)
        args <- list(...)
        if ((length(args) == 1) && is.list(args[[1]])) 
            args <- args[[1]]
        names.args <- names(args)
        i <- (1:length(names.args))[names.args == ""]
        if (length(i) > 0) 
            names.return <- unlist(args[i])
        else names.return <- unlist(args)
        names(names.return) <- NULL
        names.both <- c(names.args[names.args != ""], names.return)
        names.data <- c("keyboard", "specification.name", "observation.name", 
            "data.name", "source.name", "output.name")
        if (length(names.both) == 0) {
            names.both <- names.data
            names.return <- names.both
        }
        result <- NULL
        result <- .option.switch(result, names.data[1], names.both, 
            names.args, args, object = object)
        for (i in 1:5) result <- .option.name(result, names.data[1 + 
            i], names.both, names.args, args, c(20, 21, 22, 23, 
            27)[i], object = object)
        return(result)
    }
    type <- .return.type(object)
    result <- NULL
    if ((section == "formats") || (section == "all")) {
        results.Formats <- options.formats(..., status = status, 
            object = object)
        result <- append(result, results.Formats)
    }
    if ((section == "tests") || (section == "all")) {
        results.Tests <- options.tests(..., status = status, 
            object = object)
        result <- append(result, results.Tests)
    }
    if ((section == "ips") || (section == "all")) {
        results.IPS <- options.ips(..., status = status, object = object)
        result <- append(result, results.IPS)
    }
    if (((section == "cips") || (section == "all")) && (type == 2)) {
        results.CIPS <- options.cips(..., status = status, object = object)
        result <- append(result, results.CIPS)
    }
    if (((section == "cg") || (section == "mips") ||
        (section == "mixed") || (section == "all")) && (type == 2)) {
        results.MIXED <- options.mixed(..., status = status, 
            object = object)
        result <- append(result, results.MIXED)
    }
    if ((section == "em") || (section == "all")) {
        results.EM <- options.em(..., status = status, object = object)
        result <- append(result, results.EM)
    }
    if ((section == "exact") || (section == "all")) {
        results.Exact <- options.exact(..., status = status, 
            object = object)
        result <- append(result, results.Exact)
    }
    if ((section == "other") || (section == "all")) {
        results.Other <- options.other(..., status = status, 
            object = object)
        result <- append(result, results.Other)
    }
    if ((section == "files") || ((section == "all") && (force.files))) {
        results.Files <- options.files(..., status = status, 
            object = object)
        result <- append(result, results.Files)
    }
    return(result)
}
