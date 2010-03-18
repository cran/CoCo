"coco.simple.command" <-
function (code, sub.code = FALSE, object = .currentCoCo()) 
{
    result <- call.coco(code, ifelse(sub.code, sub.code, 0), 
        object = object)
    ok.coco(result)
}
