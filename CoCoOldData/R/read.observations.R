"read.observations" <-
function (setslot = TRUE, object = .current.coco) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.observations"))
    result <- coco.simple.command(103, FALSE, object = object)
    extractData(object = object)
    return(result)
}
