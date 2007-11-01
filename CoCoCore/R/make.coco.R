"make.coco" <-
function (n = 65536, p = 65536, q = 1024, r = 65536, s = 65536, 
    ss = 65536, t = 65536, uniq.title = FALSE, title = FALSE, 
    type = 1, location = c(700, 550), manager = TRUE, silent = FALSE, 
    sh.lib.name = NULL, ...) 
{
    coco.init(n, p, q, r, s, ss, t, TRUE, title = title, type = type, 
        silent = silent, location = location, manager = manager, 
        sh.lib.name = sh.lib.name)
    result <- .new.coco(CoCoCore::.currentCoCo(), type, uniq.title = uniq.title, 
        title = title)
    size <- c(n, p, q, r, s, ss, t)
    names(size) <- c("n", "p", "q", "r", "s", "ss", "t")
    result <- .SetSlotValue(result, ".parameters", list(size = size, 
        location = location, manager = manager, silent = silent, 
        sh.lib.name = sh.lib.name))
    my.assign(".current.coco", result, frame = 0)
    return(CoCoCore::.currentCoCo())
}
