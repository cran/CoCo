"returnNcells" <-
function (set = "*", full = FALSE, total = FALSE, data = NULL, 
    object = .object.of.thing(data = data, ...), ...) 
{
    result <- call.coco(142, ifelse(full, 2, 1) + ifelse(total, 
        2, 0), arg.char = set, arg.long = c(0), object = object)
    if (ok.coco(result)) 
        result$arg.long[1]
}
