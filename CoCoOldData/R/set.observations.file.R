"set.observations.file" <-
function (file.name, object = .current.coco) 
{
    if ("what" != file.name) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "set.observations.file", 
            name = file.name, when = "primo"))
    call.coco.chars(21, file.name, FALSE, object = object)
}
