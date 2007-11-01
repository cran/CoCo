"currentCoCo" <-
function (object = CoCoCore::.currentCoCo()) 
{
    CoCoCore::my.assign(".current.coco", object, frame = 0)
}
