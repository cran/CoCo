"disposeOfModel" <-
function (model = FALSE, a = FALSE, b = FALSE, data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    .visit.model(model, a, b, action = "dispose", object)
}
