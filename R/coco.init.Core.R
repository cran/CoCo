"coco.init" <-
function (n = 131072, p = 65536, q = 1024, r = 65536, s = 65536, 
    ss = 65536, t = 65536, init = TRUE, title = NULL, type = 1, 
    silent = FALSE,
    location = c(700, 550), manager = TRUE, sh.lib.name = NULL) 
{
    "ok.coco.start" <- function(resultat) {
        if ((resultat$ifail == .apiVersion()) | (resultat$ifail == 0)) 
            NULL
        else if ((50 <= resultat$ifail) & (resultat$ifail <= 60)) 
            ok.coco(resultat)
        else warning("Old version of CoCo object file")
    }
    if (!(exists(".coco.loaded") && .CoCoLoaded())) 
        coco.load(sh.lib.name)
    if (exists("coco.started") && .CoCoStarted()) {
        x <- .CoCoIdentifications()[, 2] == type
        if (length(x[x]) > 0) {
            id <- .CoCoIdentifications()[x, 1][1]
            if (id == .fixedCoCo()) 
                stop("This version of the shared library file of CoCo is only able to handle one CoCo-object. Use endCoCo()")
        }
    }
    my.assign("coco.started", TRUE, frame = 0)
    if (type == 2)
      sub.dir <- "library/CoCo/lib/mips"
    else
      sub.dir <- "library/CoCo/lib/coco"
    Arg.char <- .get.lib.and.tmp(sub.dir)
    ci <- .CoCoIdentifications()
    n.type <- length(ci[(ci[, 1] != .endedCoCo()) & (ci[, 2] == type), 1])
    if (n.type == 0) 
        sub.code <- NULL
    else sub.code <- n.type
    if (silent) {
        # sub.code <- 1
        capture.output(
            resultat <- call.coco(code = ifelse(init, -2, -3), 
                sub.code = sub.code, 
                arg.char = Arg.char, arg.long = c(n, p, q, r, s, ss, t), 
                    arg.double = c(my.not.a.number()), type = type, object = 0)
        )
    } else
        resultat <- call.coco(code = ifelse(init, -2, -3), 
            sub.code = sub.code, 
            arg.char = Arg.char, arg.long = c(n, p, q, r, s, ss, t), 
                arg.double = c(my.not.a.number()), type = type, object = 0)
    cat("\n")
    # my.assign(".char.ok", TRUE, frame = 0)
    my.assign(".current.coco", c(resultat$id, type), frame = 0)
    my.assign(".coco.identifications", rbind(.CoCoIdentifications(), 
        object = .currentCoCo()), frame = 0)
    ok.coco.start(resultat)
}
