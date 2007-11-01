"coco.enter.all" <-
function (code, string, long, double, sub.code = FALSE, object = CoCoCore::.currentCoCo()) 
{
    "coco.set.all.options" <- function(code, string, long, double, 
        sub.code = FALSE, object = CoCoCore::.currentCoCo()) {
        if (is.vector(string)) 
            string <- string[1]
        if (is.na(string)) string <- ""
        if ((sub.code && (string != "what"))) {
            result <- call.coco(code, sub.code, arg.char = string, 
                arg.long = long, arg.double = double, object = object)
        }
        else if (is.character(string) && (string != "what")) {
            result <- call.coco(code, 0, arg.char = c(string), 
                arg.long = long, arg.double = double, object = object)
        }
        else if ((string == "what") || (string == FALSE)) {
            tmp <- call.coco(code, ifelse(sub.code, sub.code, 
                -1), arg.char = .empty.string(20), arg.long = rep(0, 
                10), arg.double = rep(-1, 1), object = object)
            if (70 == tmp$ifail[1]) 
                result <- call.coco(code, ifelse(sub.code, sub.code, 
                  -1), arg.char = .empty.string(tmp$n.args[1]), 
                  arg.long = rep(0, tmp$n.args[2]), arg.double = rep(-1, 
                    tmp$n.args[3]), object = object)
            else result <- tmp
        }
        else {
            result <- call.coco(code, 0, arg.char = string, arg.long = long, 
                arg.double = double, object = object)
        }
        if (ok.coco(result)) {
            if (result$n.args[1] > 1) {
                string <- substring(result$arg.char, 1, (result$n.args[1] - 
                  1))
            }
            else string <- NULL
            if (result$n.args[2] > 0) {
                long <- result$arg.long[1:(result$n.args[2] - 
                  1)]
                if (long[length(long)] == 2147483647) 
                  long <- long[-length(long)]
            }
            else long <- NULL
            if (result$n.args[3] > 0) {
                double <- result$arg.double[1:(result$n.args[3] - 
                  1)]
            }
            else double <- NULL
            list(string, long, double)
        }
        else NULL
    }
    result <- coco.set.all.options(code, string, long, double, 
        sub.code, object = object)
    if (((string == "what") || !is.character(string))) 
        result
    else result
}
