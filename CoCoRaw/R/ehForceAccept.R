"ehForceAccept" <-
function (model, a = FALSE, b = FALSE, data = NULL, object = .object.of.model(model, 
    data = data, ...), ...) 
{
    if ("what" == model) 
        coco.enter.string(218, "what", FALSE, object = object)
    else if ((model == "models") & is.gc(a)) 
        coco.enter.string(218, a, FALSE, object = object)
    else if ((model == "r.dual") | (model == "a.dual")) 
        add.dual.to.class(model, "accepted", a, object = object)
    else if ((model == "base") | (model == "current") | (model == 
        "last") | (model == "all") | (model == "interval") | 
        (model == "list") | is.number(model) | is.vector(model)) 
        .to.search("accept", model, a, b, object = object)
    else if (is.gc(model)) 
        coco.enter.string(218, model, FALSE, object = object)
    else .to.search("accept", model, a, b, object = object)
}
