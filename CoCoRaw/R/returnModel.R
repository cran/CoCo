"returnModel" <-
function (model = "current", type = "both", as.edges = FALSE, 
	  split.string = FALSE, split.generators = FALSE, data = NULL, 
          object = .object.of.model(model, data = data, ...), ...) 
{
    if (as.edges)
        returnEdges(model = model, object = object, ...)
    else {
        simple.model <- c("base", "current", "last")
        coco.type <- .return.type(object = object)
        if (!((coco.type == 2) && any(model == simple.model))) 
            old.current <- .before.set.current(model, object = object)
        if (coco.type == 2) {
            t <- .encode(c("gc", "cs", "both", "discrete", "linear", 
                "quadratic"), type, -(1:6), -3)
            model <- .recover.model(model)
            m <- .encode(simple.model, model, -(1:3), -2)
            result <- coco.enter.string(127, "what", 10 * t + m, 
                object = object)
        }
        else {
            result <- coco.enter.string(127, "what", -2, object = object)
        }
        if (!((coco.type == 2) && any(model == simple.model))) 
            result <- .after.set.current(old.current, result, 
					 type = "unconditioned", 
					 model = FALSE, object = object)
        if ((split.string || split.generators)) 
            result <- .split.model.gc(result, split.generators = split.generators)
        return(result)
        }
}
