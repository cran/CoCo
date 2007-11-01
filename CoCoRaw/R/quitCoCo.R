"quitCoCo" <-
function (..., object = CoCoCore::.currentCoCo()) 
{
    if (exists(".fewer.warnings")) 
        fewer.warnings <- CoCoCore::.fewerWarnings()
    else fewer.warnings <- 0
    if ((exists("coco.started") && CoCoCore::.CoCoStarted())) {
        if (fewer.warnings == 0) 
            warning("CoCo not ended")
        for (i in 1:nrow(CoCoCore::.CoCoIdentifications())) {
            result <- CoCoCore::.CoCoIdentifications()[i, ]
            my.assign("coco.started", TRUE, frame = 0)
            if (result[1] != CoCoCore::.endedCoCo()) 
                endCoCo(result)
        }
    }
    my.assign("coco.started", FALSE, frame = 0)
    my.assign(".coco.loaded", FALSE, frame = 0)
    .clear.coco.objects(silent = TRUE, pos = .GlobalEnv)
    invisible()
    q(...)
}
