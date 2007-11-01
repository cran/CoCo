"read.observations" <-
function (setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.observations"))
    result <- coco.simple.command(103, FALSE, object = object)
    CoCoRaw::extractData(object = object)
    return(result)
}
