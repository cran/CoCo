"makeCurrentCoCo" <-
function (object.number) 
{
    if ((class(object.number) == "CoCoModelClass") || (class(object.number) == 
        "CoCoClass")) {
        my.assign(".current.coco", object.number, frame = 0)
    }
    else if ((class(object.number) == "numeric") && (length(object.number) == 
        1)) {
        my.assign(".current.coco", .coco.identifications[object.number, 
            ], frame = 0)
        my.assign("coco.started", TRUE, frame = 0)
    }
}
