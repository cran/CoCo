"makeModel" <-
function (model = FALSE, title = "", push.pop = FALSE, data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    make.model(model, title = title, push.pop = push.pop, object = object)
}
