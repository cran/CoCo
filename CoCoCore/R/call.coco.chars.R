"call.coco.chars" <-
function (code, argument = FALSE, sub.code = FALSE, 
    length = 128, no.warnings = NULL, object = .current.coco) 
{
    if (is.vector(argument)) 
        argument <- argument[1]
    if ((sub.code && (argument != "what"))) {
        result <- call.coco(code, sub.code, arg.char = argument, 
            object = object)
    }
    else if (is.character(argument) && (argument != "what")) {
        result <- call.coco(code, 0, arg.char = c(argument), 
            object = object)
    }
    else if ((argument == "what") || (argument == FALSE)) {
        tmp <- call.coco(code, ifelse(sub.code, sub.code, -1), 
            arg.char = .empty.string(length), object = object)
        if (70 == tmp$ifail[1]) 
            result <- call.coco(code, ifelse(sub.code, sub.code, -1), 
                arg.char = .empty.string(tmp$n.args[1]), 
                object = object)
        else result <- tmp
    }
    else {
        result <- call.coco(code, 0, arg.char = argument, object = object)
    }
    ok <- ok.coco(result, no.warnings)
    if (ok) 
        result$arg.char
    else NULL
}
