"call.coco.message" <-
function (code, sub.code = 0, arg.char = "", arg.long = NULL, 
    arg.double = NULL, object = CoCoCore::.currentCoCo()) 
{
    result <- call.coco(code, sub.code, arg.char = arg.char, 
        arg.long = arg.long, arg.double = arg.double, object = object)
    ok.coco(result)
}
