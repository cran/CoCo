"read.data" <-
function (setslot = TRUE, object = .current.coco) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.data"))
    result <- coco.simple.command(88, FALSE, object = object)
    cat("\n")
    extractData(object = object)
    return(result)
}
