"set.data.file" <-
function (file.name, object = .current.coco) 
{
    if ("what" != file.name) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "set.data.file", 
            name = file.name, when = "primo"))
    call.coco.chars(22, file.name, FALSE, object = object)
}
