"returnModelVariates" <-
function (model = FALSE, as.string = TRUE, data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    if (as.string) {
        warning("This method returns a string, not the list of integers: '(0) 1 for variables (not) in the model'; ")
        result <- coco.enter.string(141, "what", -2, object = object)
    }
    else result <- call.coco.longs(140, "what", numberVariates(full = FALSE, 
        object = object), -2, object = object)
    .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
}
