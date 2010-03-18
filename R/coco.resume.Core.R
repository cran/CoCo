"coco.resume" <-
function (object = .currentCoCo()) 
{
    if (!(exists("coco.started") && .CoCoStarted())) 
        coco.start()
    else ok.coco(call.coco(-1, NULL, object = object))
}
