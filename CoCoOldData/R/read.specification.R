"read.specification" <-
function (setslot = TRUE, object = .current.coco) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.specification"))
    coco.simple.command(89, FALSE, object = object)
}
