"read.names" <-
function (setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.names"))
    coco.simple.command(91, FALSE, object = object)
}
