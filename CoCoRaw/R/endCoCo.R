"endCoCo" <-
function (object = .current.coco, silent = FALSE) 
{
    cid.arg <- .coco.id(object = object, recover = FALSE)
    typ.arg <- .return.type(object = object)
    cid <- .coco.identifications
    for (i in 1:nrow(cid))
      if ((cid.arg == cid[i, 1]) && (typ.arg == cid[i, 2]))
        cid[i, ] <- c(.ended.coco, 0)
    n <- length(cid[(cid[, 1] != .ended.coco) & (cid[, 2] == typ.arg), 1])
    if ((exists("coco.started") && coco.started)) 
        call.coco(0, n, object = object)
    .clear.coco.objects(coco.object = object, silent = silent)
    if (.return.reference(.current.coco) == cid.arg) 
        my.assign(".current.coco", .ended.coco, frame = 0)
    my.assign(".coco.identifications", cid, frame = 0)
    my.assign("coco.started", FALSE, frame = 0)
    invisible()
}
