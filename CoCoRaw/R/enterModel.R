"enterModel" <-
function (model = "*", order = NULL, set = "*", homogeneous = FALSE, 
    data = NULL, object = .object.of.thing(data = data, ...), 
    ...) 
{
    if ((is.character(set) & (length(order) == 1))) {
        call.coco.message(128, FALSE, arg.char = set, arg.long = order, 
            object = object)
    }
    else {
        type <- .return.type(object)
        if ((type != 2)) 
            coco.enter.string(127, model, 1, object = object)
        else if ((model == "*")) 
            coco.enter.string(127, model, ifelse(homogeneous, 
                3, 4), object = object)
        else coco.enter.string(127, model, 2, object = object)
    }
}
