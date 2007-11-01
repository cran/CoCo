"read.specification" <-
function (setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.specification"))
    coco.simple.command(89, FALSE, object = object)
}
