"ehFindDual" <-
function (dual = "both", sub.class = FALSE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    coco.simple.command(224, sum(c(.encode(c("a.dual", "r.dual", 
        "both", "both.duals"), dual, c(1, 2, 5, 5), dual), .encode(c("decomposable", 
        "graphical", "hierarchical"), sub.class, c(10, 20, 30), 
        0))), object = object)
}
