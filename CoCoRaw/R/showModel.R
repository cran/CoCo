"showModel" <-
function (model = FALSE, a = FALSE, b = FALSE, describe.model = FALSE, 
    data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    if (describe.model) 
        .visit.model(model, a, b, action = "describe", object)
    else .visit.model(model, a, b, action = "show", object)
}
