"call.coco" <-
function (code, sub.code = 0, arg.char = "", arg.long = NULL, 
    arg.double = NULL, object = .currentCoCo(), char.ok = FALSE, 
    type = NULL) 
{
    ".call.coco" <- function(code, sub.code = 0, arg.char = "", 
        arg.long = NULL, arg.double = NULL, arg.char.int = NULL, 
        type = NULL, object = .currentCoCo()) {
        ok <- .coco.command.implemented(code = code, sub.code = sub.code, 
            arg.char = arg.char, arg.long = arg.long, arg.double = arg.double, 
            arg.char.int = NULL, object = object)
        if (ok) 
            id <- .coco.id(object = object, recover = TRUE)
        else id <- FALSE
        if (!(exists("coco.started") && .CoCoStarted())) {
            warning("CoCo not started!!!")
        }
        if (is.numeric(id)) {
            n.arg <- c(nchar(arg.char), length(arg.long), length(arg.double), 
                length(arg.char.int))
            if (length(type) == 0) 
                type <- .return.type(object = object)
            name <- ifelse((type == 2), "Mips", "CoCo")
            PACKAGE <- ifelse((type == 2), "CoCoCg", "CoCo")
            if (exists("trace.call.coco") && .traceCallCoCo()) {
                print(paste(".call.coco: ", code, sub.code, arg.long[1]))
            }
            result <- .C(name = name, PACKAGE = PACKAGE, 
                ifail = as.integer(.noIfail()), 
                id = as.integer(id), code = as.integer(code), 
                sub.code = as.integer(sub.code), 
                n.args = as.integer(n.arg), 
                arg.char = as.character(arg.char), 
                arg.long = as.integer(arg.long), 
                arg.double = as.double(arg.double), 
                arg.charint = as.integer(arg.char.int), 
                NAOK = TRUE, specialsok = FALSE, COPY = c(FALSE, 
                  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                  FALSE), CLASSES = c("integer", "integer", "integer", 
                  "integer", "integer", "character", "integer", 
                  "double", "integer"))
            return(result)
        }
    }
    ".string.to.int" <- function(a) {
        ".char.int" <- function(c) {
            match(c, .my.ascii()) - 1
        }
        if (a != "") 
            if (is.character(a)) 
                .char.int(substring(a, 1:nchar(a), 1:nchar(a)))
            else {
                warning("Invalid argument")
                NULL
            }
        else NULL
    }
    ".int.to.string" <- function(a) {
        ".int.char" <- function(i) {
            .my.ascii()[i[i != 0] + 1]
        }
        if (exists(".fewer.warnings")) 
            fewer.warnings <- .fewerWarnings()
        else fewer.warnings <- 0
        if (any(a == 127)) 
            if (all(a == 127 | a == 0)) {
                if (fewer.warnings == 0) 
                  warning(paste("Missing 'end'-mark - empty string, '", 
                    paste(.int.char(a), collapse = ""), 
                    "' returned as 'NULL'"))
                return(NULL)
            }
            else {
                b <- max((1:length(a))[a != 127 & a != 0])
                if (0 < b) 
                  r <- paste(.int.char(a[1:b]), collapse = "")
                else r <- NULL
                if (fewer.warnings == 0) 
                  warning(paste("Missing 'end'-mark, '", paste(.int.char(a), 
                    collapse = ""), "' returned as '", r, "'"))
                return(r)
            }
        else paste(.int.char(a), collapse = "")
    }
    if (exists(".char.ok")) 
        char.ok <- .charOk()
    if (char.ok) 
        .call.coco(code, sub.code, arg.char = paste(arg.char, 
            ";", collapse = ""), arg.long = arg.long, arg.double = arg.double, 
            type = type, object = object)
    else {
        result <- .call.coco(code, sub.code, arg.char = "", 
            arg.long = arg.long, 
            arg.double = arg.double, 
            arg.char.int = c(.string.to.int(arg.char), 
                0), type = type, object = object)
        result$n.args[1] <- result$n.args[4]
        .String <- .int.to.string(result$arg.charint)
        if (length(.String) == 0) 
            .String <- ""
        result$arg.char <- .String
        result
    }
}
