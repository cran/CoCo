"extractData" <-
function (full = FALSE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    names <- .return.name.list.string(full = full, object = object)
    levels <- .return.level.list(full = full, object = object)
    missing <- .return.missing.list(full = full, object = object)
    Levels <- levels - missing
    result <- .set.coco.value(object, ".specification", list(type = "names", 
        names = substr(names, 2, nchar(names) - 1), levels = Levels, 
        missing = missing))
    type <- .return.type(object)
    datastructure <- .set.datastructure("what", object)
    if ((type != 2)) {
        if (((datastructure == "all") || (datastructure == "necessary")) && 
            prod(levels) < 2^20) {
            counts <- returnTable("observed", set = names, object = object)
            result <- .set.coco.value(object, ".observations", 
                list(type = "table", counts = counts, silent = TRUE))
        }
        else {
            case.list <- returnTable("sparse.table", set = names, 
                object = object)
            result <- .set.coco.value(object, ".observations", 
                list(type = "list", list = case.list$long, accumulated = TRUE, 
                  ncol = length(levels), select.case.fun = NULL, 
                  columns = 1:length(levels), silent = TRUE))
        }
    }
    else warning("Unable to extract the data of this object to slots.")
    return(result)
}
