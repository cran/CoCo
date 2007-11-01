"endCoCo" <-
function (object = CoCoCore::.currentCoCo(), silent = FALSE) 
{
    cid.arg <- .coco.id(object = object, recover = FALSE)
    typ.arg <- .return.type(object = object)
    cid <- CoCoCore::.CoCoIdentifications()
    for (i in 1:nrow(cid))
      if ((cid.arg == cid[i, 1]) && (typ.arg == cid[i, 2]))
        cid[i, ] <- c(CoCoCore::.endedCoCo(), 0)
    n <- length(cid[(cid[, 1] != CoCoCore::.endedCoCo()) & (cid[, 2] == typ.arg), 1])
    if ((exists("coco.started") && CoCoCore::.CoCoStarted())) 
        call.coco(0, n, object = object)
    .clear.coco.objects(coco.object = object, silent = silent)
    if (.return.reference(CoCoCore::.currentCoCo()) == cid.arg) 
        my.assign(".current.coco", CoCoCore::.endedCoCo(), frame = 0)
    my.assign(".coco.identifications", cid, frame = 0)
    my.assign("coco.started", FALSE, frame = 0)
    invisible()
}
