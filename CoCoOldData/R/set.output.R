"set.output" <-
function (file.name, object = CoCoCore::.currentCoCo()) 
{
    call.coco.chars(27, file.name, FALSE, object = object)
}
