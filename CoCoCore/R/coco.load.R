"coco.load" <-
function (sh.lib.name = NULL) 
{
    ".coco.load" <- function(sh.lib.name = NULL) {
        if (!is.loaded(symbol.C("CoCo"))) {
            if (is.null(sh.lib.name)) 
                sh.lib.name <- paste(.get.rs.cshlib.name("RSCSHLIB"), 
                  .platform.dynlib.ext, sep = "")
            Xsh.lib.name <- paste("X", sh.lib.name, sep = "")
            Xfile <- file.path(.get.cocolib.name("COCOLIB"), 
                Xsh.lib.name)
            if (.my.test.file.found(Xfile)) 
                sh.lib.name <- Xsh.lib.name
            file <- file.path(.get.cocolib.name("COCOLIB"), sh.lib.name)
            .my.dyn.load.open(file)
        }
    }
    my.assign(".fewer.warnings", 1, frame = 0)
    my.assign(".api.version", 140, frame = 0)
    my.assign(".fixed.coco", 2147483646, frame = 0)
    my.assign(".ended.coco", 2147483647, frame = 0)
    my.assign(".no.ifail", 0, frame = 0)
    my.assign(".coco.identifications", NULL, frame = 0)
    my.assign(".instances.coco", NULL, frame = 0)
    my.assign(".instances.coco.models", NULL, frame = 0)
    my.assign(".current.coco", c(.ended.coco, 0), frame = 0)
    my.assign(".coco.loaded", TRUE, frame = 0)
    .coco.load(sh.lib.name)
}
