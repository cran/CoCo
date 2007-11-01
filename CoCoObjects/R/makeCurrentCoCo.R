"makeCurrentCoCo" <-
function (object.number) 
{
    if ((class(object.number) == "CoCoModelClass") || (class(object.number) == 
        "CoCoClass")) {
        CoCoCore::my.assign(".current.coco", object.number, frame = 0)
    }
    else if ((class(object.number) == "numeric") && (length(object.number) == 
        1)) {
        CoCoCore::my.assign(".current.coco", CoCoCore::.CoCoIdentifications()[object.number, 
            ], frame = 0)
        CoCoCore::my.assign("coco.started", TRUE, frame = 0)
    }
}
