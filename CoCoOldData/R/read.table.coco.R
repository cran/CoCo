"read.table.coco" <-
function (silent = FALSE, setslot = TRUE, object = .current.coco) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.table"))
    result <- coco.simple.command(104, FALSE, object = object)
    cat("\n")
    extractData(object = object)
    return(result)
}
