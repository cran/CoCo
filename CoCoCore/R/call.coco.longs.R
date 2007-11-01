"call.coco.longs" <-
function (code, argument = FALSE, length = 0, sub.code = FALSE, 
    no.warnings = NULL, object = CoCoCore::.currentCoCo()) 
{
    if (is.vector(argument)) 
        tst <- argument[1]
    else tst <- argument
    if (is.na(tst)) tst <- ""
    if ((sub.code && (tst != "what"))) {
        result <- call.coco(code, sub.code, arg.long = c(argument), 
            object = object)
    }
    else if (is.number(tst)) {
        result <- call.coco(code, 0, arg.long = c(argument), 
            object = object)
    }
    else if ((tst == "what") || (tst == FALSE)) {
        tmp <- call.coco(code, ifelse(sub.code, sub.code, -1), 
            arg.long = rep(0, length), object = object)
        if (70 == tmp$ifail[1]) 
            result <- call.coco(code, ifelse(sub.code, sub.code, -1), 
                arg.long = rep(0, tmp$n.args[2]), object = object)
        else result <- tmp
    }
    else {
        result <- call.coco(code, 0, arg.long = argument, object = object)
    }
    ok <- ok.coco(result, no.warnings)
    if (ok) 
        result$arg.long
    else NULL
}
