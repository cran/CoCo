"read.list" <-
function (silent = FALSE, setslot = TRUE, object = .current.coco) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.list"))
    result <- coco.simple.command(105, FALSE, object = object)
    extractData(object = object)
    return(result)
}
