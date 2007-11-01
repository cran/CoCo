"coco.resume" <-
function (object = CoCoCore::.currentCoCo()) 
{
    if (!(exists("coco.started") && CoCoCore::.CoCoStarted())) 
        coco.start()
    else ok.coco(call.coco(-1, NULL, object = object))
}
