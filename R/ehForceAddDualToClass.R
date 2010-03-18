"ehForceAddDualToClass" <-
function (dual = "a.dual", class = "accepted", sub.class = FALSE, 
    data = NULL, object = .object.of.thing(data = data, ...), 
    ...) 
{
    coco.simple.command(227, sum(c(.encode(c("r.dual", "a.dual"), 
        dual, c(1, 2), dual), .encode(c("accepted", "rejected"), 
        class, c(0, 2), class), .encode(c("decomposable", "graphical", 
        "hierarchical"), sub.class, c(10, 20, 30), 0))), object = object)
}
