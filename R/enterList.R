"enterList" <-
function (discrete, continuous = NULL, accumulated = FALSE, 
    # ncol = FALSE, select.case.fun = FALSE, columns = FALSE, silent = FALSE, 
    setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
        ...), ...) 
{
    if (length(continuous) > 0) 
        enterTwoLists(discrete, continuous, accumulated = accumulated, 
            # ncol = ncol, select.case.fun = select.case.fun, columns = columns, 
            # silent = silent, 
            object = object)
    else {
        x <- length(.return.level.list(full = TRUE, object = object)) + 
            ifelse(accumulated, 1, 0)
        .set.coco.value(object, ".observations", list(type = "discrete", 
            discrete = discrete, 
            # ncol = ncol, select.case.fun = select.case.fun, columns = columns, 
            # silent = silent, 
            accumulated = accumulated))
        result <- call.coco.longs(105, discrete, length(discrete), 
                                  ifelse(accumulated, 2, 1), object = object)
	if (is.null(result))
           return(NULL)
        else
           return(TRUE)
    }
}
