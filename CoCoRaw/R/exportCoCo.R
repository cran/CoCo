"exportCoCo" <-
function (file.name, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    type <- .return.type(object)
    call.coco.chars(14, file.name, FALSE, object = object)
}
