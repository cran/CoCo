"read.list" <-
function (silent = FALSE, setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.list"))
    result <- coco.simple.command(105, FALSE, object = object)
    CoCoRaw::extractData(object = object)
    return(result)
}
