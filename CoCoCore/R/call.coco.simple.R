"call.coco.simple" <-
function (code, sub.code = FALSE, object = .current.coco) 
{
    result <- call.coco(code, ifelse((sub.code == "what") || 
        (sub.code == FALSE), -1, sub.code), object = object)
    if (ok.coco(result)) 
        result$sub.code
    else NULL
}
