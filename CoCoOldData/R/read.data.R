"read.data" <-
function (setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.data"))
    result <- coco.simple.command(88, FALSE, object = object)
    cat("\n")
    CoCoRaw::extractData(object = object)
    return(result)
}
