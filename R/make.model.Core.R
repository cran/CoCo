"make.model" <-
function (model = "*", title = "", push.pop = FALSE, object = .currentCoCo()) 
{
    if (is.gc(model)) {
        enterModel(model, object = object)
        x <- returnModelNumber("last", object = object)
    }
    else {
        x <- model
        model <- returnModel(model, object = object)
        if (!is.number(x)) 
            x <- returnModelNumber(x, pop = push.pop, object = object)
    }
    title <- ifelse(title != "", title, model)
    my.assign(".current.coco.model", .new.coco.model(x, model, 
        object = object, title = title), frame = 0)
    .currentCoCoModel()
}
