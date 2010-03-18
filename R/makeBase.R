"makeBase" <-
function (model = "current", silent = TRUE, both = FALSE, push = FALSE, 
    data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    number <- .encode.model.1(model, object = object)
    result <- call.coco.longs(132, number, 1, sub.code = ifelse(silent, 
        1, 0) + ifelse(both, 2, 0) + ifelse(push, 1024, 0), object = object)
    if (result == number) 
        result
    else FALSE
}
