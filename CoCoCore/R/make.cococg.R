"make.cococg" <-
function (n = 65536, p = 65536, q = 1024, r = 65536, s = 65536, 
    ss = 65536, t = 65536, uniq.title = FALSE, title = FALSE, 
    location = c(700, 550), manager = TRUE, silent = FALSE,
    sh.lib.name = NULL) 
{
    # require(CoCoCg)
    message("'require(CoCoCg)' missing!")
    make.coco(n, p, q, r, s, ss, t, TRUE, uniq.title = uniq.title, 
        title = title, type = 2, location = location, manager = manager, 
        silent = silent, sh.lib.name = sh.lib.name)
}
