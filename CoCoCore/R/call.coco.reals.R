"call.coco.reals" <-
function (code, argument = FALSE, length = 0, sub.code = FALSE, 
    no.warnings = NULL, object = .current.coco) 
{
    if (is.vector(argument)) 
        tst <- argument[1]
    else tst <- argument
    if ((sub.code && (tst != "what"))) {
        result <- call.coco(code, sub.code, arg.double = c(argument), 
            object = object)
    }
    else if (is.number(tst)) {
        result <- call.coco(code, 0, arg.double = c(argument), 
            object = object)
    }
    else if ((tst == "what") || (tst == FALSE)) {
        tmp <- call.coco(code, ifelse(sub.code, sub.code, -1), 
            arg.double = rep(as.double(0), length), object = object)
        if (70 == tmp$ifail[1]) 
            result <- call.coco(code, ifelse(sub.code, sub.code, -1), 
                arg.double = rep(0, tmp$n.args[3]), object = object)
        else result <- tmp
    }
    else {
        result <- call.coco(code, 0, arg.double = argument, object = object)
    }
    ok <- ok.coco(result, no.warnings)
    if (ok) 
        result$arg.double
    else NULL
}
