"quitCoCo" <-
function (..., object = .current.coco) 
{
    if (exists(".fewer.warnings")) 
        fewer.warnings <- .fewer.warnings
    else fewer.warnings <- 0
    if ((exists("coco.started") && coco.started)) {
        if (fewer.warnings == 0) 
            warning("CoCo not ended")
        for (i in 1:nrow(.coco.identifications)) {
            result <- .coco.identifications[i, ]
            my.assign("coco.started", TRUE, frame = 0)
            if (result[1] != .ended.coco) 
                endCoCo(result)
        }
    }
    my.assign("coco.started", FALSE, frame = 0)
    my.assign(".coco.loaded", FALSE, frame = 0)
    .clear.coco.objects(silent = TRUE, pos = .GlobalEnv)
    invisible()
    q(...)
}
