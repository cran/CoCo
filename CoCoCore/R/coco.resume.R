"coco.resume" <-
function (object = .current.coco) 
{
    if (!(exists("coco.started") && coco.started)) 
        coco.start()
    else ok.coco(call.coco(-1, NULL, object = object))
}
