"importCoCo" <-
function (file.name, setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    .set.coco.value(object, ".specification", list(type = "import", 
        file.name = file.name))
    type <- .return.type(object)
    if (type == 2) 
        call.coco.chars(15, file.name, FALSE, object = object)
}
