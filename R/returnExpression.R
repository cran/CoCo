"returnExpression" <-
function (model = "current", type = "expression", omit.prime.components = FALSE, 
    omit.separators = FALSE, omit.generators = FALSE, state.space = FALSE, 
    return.flags = FALSE, split.sets = FALSE, split.models = TRUE, 
    split.generators = TRUE, eliminate.empty = TRUE, data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    result <- .return.complex(model = model, type = type, 
        omit.prime.components = omit.prime.components, 
        omit.separators = omit.separators, 
        omit.generators = omit.generators, state.space = state.space, 
        return.flags = return.flags, split.sets = split.sets, 
        split.models = split.models, split.generators = split.generators, 
        eliminate.empty = eliminate.empty, data = data, object = object)
    .end.temporary.object(model, data = data, ...)
    return(result)
}
