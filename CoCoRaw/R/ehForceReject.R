"ehForceReject" <-
function (model, a = FALSE, b = FALSE, data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    if ("what" == model) 
        coco.enter.string(219, "what", FALSE, object = object)
    else if ((model == "models") | is.gc(a)) 
        coco.enter.string(219, a, FALSE, object = object)
    else if ((model == "r.dual") | (model == "a.dual")) 
        add.dual.to.class(model, "rejected", a, object = object)
    else if ((model == "base") | (model == "current") | (model == 
        "last") | (model == "all") | (model == "interval") | 
        (model == "list") | is.number(model) | is.vector(model)) 
        .to.search("reject", model, a, b, object = object)
    else if (is.gc(model)) 
        coco.enter.string(219, model, FALSE, object = object)
    else .to.search("reject", model, a, b, object = object)
}
