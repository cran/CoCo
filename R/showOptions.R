"showOptions" <-
function (hit = "all", dump = FALSE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    x <- c("all", "formats", "tests", "exact", "fix", "ips", 
        "cips", "mips", "em", "old", "specification", "factors", 
        "observations", "data", "limits", "files", "other", "search", 
        "eh")
    y <- c(1, 2, 3, 4, 5, 6, 6, 6, 7, 18, 8, 8, 9, 9, 9, 10, 
        11, 12, 12)
    if (dump) {
        type <- .return.type(object)
        if (type != 2) 
            dump <- FALSE
    }
    coco.simple.command(ifelse(dump, 12, 13), .encode(x, hit, 
        y, 1), object = object)
}
