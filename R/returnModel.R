"returnModel" <-
function (model = "current", type = "both", as.edges = FALSE, 
	  split.string = FALSE, split.generators = FALSE, data = NULL, 
          object = .object.of.model(model, data = data, ...), ...) 
{
    insert.bar <- function(a) {
      if (length(grep("\\]\\]\\[\\[\\]\\]", a)) > 0)
        return(sub("\\]\\]\\[\\[\\]\\]", "\\]\\] ;", a))
      else if (length(grep("\\]\\]\\[\\[", a)) > 0)
        return(sub("\\]\\]\\[\\[", "\\] | \\[\\[", sub("\\[\\[", "\\[", a)))
      else
        return(a)
    }

    if (as.edges)
        returnEdges(model = model, object = object, ...)
    else {
        bcl <- c("base", "current", "last")
        simple.model <- FALSE
        if (is.character(model))
            simple.model <- any(model == bcl)
        coco.type <- .return.type(object = object)
        if (!((coco.type == 2) && simple.model)) 
            old.current <- .before.set.current(model, object = object)
        if (coco.type == 2) {
            t <- .encode(c("gc", "cs", "both", "discrete", "linear", 
                "quadratic"), type, -(1:6), -3)
            if (simple.model) {
                rmodel <- .recover.model(model)
if (!is.character(model)) {
print(model)
print(rmodel)
}
                m <- .encode(bcl, rmodel, -(1:3), -2)
            }
            else {
                m <- -2
            }
            result <- coco.enter.string(127, "what", 10 * t + m, 
                object = object)
        }
        else {
            result <- coco.enter.string(127, "what", -2, object = object)
        }
        if (!((coco.type == 2) && simple.model)) 
            result <- .after.set.current(old.current, result, 
					 type = "unconditioned", 
					 model = FALSE, object = object)
        if ((coco.type == 2) && (type == "both"))
            result <- insert.bar(result)
        if ((split.string || split.generators)) 
            result <- .split.model.gc(result, 
                                      split.generators = split.generators)
        return(result)
        }
}
