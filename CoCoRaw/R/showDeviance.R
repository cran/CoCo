"showDeviance" <-
function (model.1 = "current", model.2 = "base", data = NULL, 
    object = .object.of.models(model.1, model.2, data = data, 
        ...), ...) 
{
    coco.simple.double(162, FALSE, model.1, model.2, type = "unconditioned", 
        object = object)
}
