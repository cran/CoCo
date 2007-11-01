"set.specification.file" <-
function (file.name, object = CoCoCore::.currentCoCo()) 
{
    if ("what" != file.name) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "set.specification.file", 
            name = file.name, when = "primo"))
    call.coco.chars(20, file.name, FALSE, object = object)
}
