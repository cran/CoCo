"numberVariates" <-
function (full = FALSE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    result <- call.coco.longs(146, 0, 1, ifelse(full, 2, 1), 
        object = object)
    ifelse(result, result, FALSE)
}
