"skip.missing" <-
function (object = .current.coco) 
{
    .set.coco.value(object, ".invalid", append = TRUE, list(type = "skip.missing", 
        when = "medio"))
    coco.simple.command(102, FALSE, object = object)
}
