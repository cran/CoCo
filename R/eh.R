"eh" <-
function (strategy = c("smallest", "alternating", "rough")[1],
	  sub.class = c("decomposable", "graphical", "hierarchical")[2],
	  p.accepted = FALSE, 
	  data = NULL, object = .object.of.thing(data = data, ...), 
	  ...) 
{
    if (p.accepted) 
        .set.acceptance(p.accepted, object = object)
    coco.simple.command(226, sum(c(.encode(c("smallest", "alternating", 
        "rough"), strategy, 1:3, 5), .encode(c("decomposable", 
        "graphical", "hierarchical"), sub.class, c(10, 20, 30), 
        0))), object = object)
}
