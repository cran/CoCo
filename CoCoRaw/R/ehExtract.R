"ehExtract" <-
function (class, sub.class = FALSE, data = NULL, object = .object.of.thing(data = data, 
    ...), ...) 
{
    coco.simple.command(220, sum(c(.encode(c("accepted", "rejected", 
        "a.duals", "r.duals", "a.dual", "r.dual"), class, c(1:4, 3:4), class),
	.encode(c("decomposable", "graphical", "hierarchical"), 
				  sub.class, c(10, 20, 30), 0))), object = object)
}
