"coco.replace.all" <-
function (code, string = "", long = NULL, double = NULL, sub.code = FALSE, 
    object = .current.coco) 
{
    if (is.vector(string)) 
        string <- string[1]
    .arg.char <- paste(c(string, ";", .empty.string(0)), collapse = "")
    tmp <- call.coco(code, ifelse(sub.code, sub.code, -1), arg.char = .arg.char, 
        arg.long = c(long, rep(0, 0)), arg.double = c(double, 
            rep(-1, 0)), object = object)
    if (70 == tmp$ifail[1]) {
        n.char <- tmp$n.args[1] - nchar(string)
        if (n.char > 0) 
            .arg.char <- paste(c(string, ";", .empty.string(n.char)), 
                collapse = "")
        else .arg.char <- paste(c(string, ";"), collapse = "")
        length.long <- tmp$n.args[2] - length(long)
        if (length.long > 0) 
            .arg.long <- c(long, rep(0, length.long))
        else .arg.long <- long
        length.double <- tmp$n.args[3] - length(double)
        if (length.double > 0) 
            .arg.double <- c(double, rep(-1, length.double))
        else .arg.double <- double
        .tmp <- call.coco(code, ifelse(sub.code, sub.code, -1), 
            arg.char = .arg.char, arg.long = .arg.long, arg.double = .arg.double, 
            object = object)
    }
    else .tmp <- tmp
    if (ok.coco(.tmp)) {
        result <- NULL
        if (.tmp$n.args[1] > 1) {
            string <- substring(.tmp$arg.char, 1, (.tmp$n.args[1] - 
                1))
            result <- append(result, list(string = string))
        }
        if (.tmp$n.args[2] > 1) {
            long <- .tmp$arg.long[1:(.tmp$n.args[2] - 1)]
            result <- append(result, list(long = long))
        }
        if (.tmp$n.args[3] > 1) {
            double <- .tmp$arg.double[1:(.tmp$n.args[3] - 1)]
            result <- append(result, list(double = double))
        }
        result
    }
    else NULL
}
