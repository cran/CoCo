"read.factors" <-
function (setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.factors"))
    coco.simple.command(90, FALSE, object = object)
}
