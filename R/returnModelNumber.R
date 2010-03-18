"returnModelNumber" <-
function (model = FALSE, no.warnings = FALSE, pop = FALSE, data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    x <- .return.object.model.number(model, recover = TRUE, object = object)
    if (x) 
        x
    else {
        x <- .encode.model(model)
        if (x < 0) {
            x <- call.coco.longs(133, 0, 1, x + sign(x) * ifelse(pop, 
                2048, 0), no.warnings = no.warnings, object = object)
            x
        }
        else x
    }
}
