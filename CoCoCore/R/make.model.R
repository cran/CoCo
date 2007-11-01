"make.model" <-
function (model = "*", title = "", push.pop = FALSE, object = CoCoCore::.currentCoCo()) 
{
    if (is.gc(model)) {
        CoCoRaw::enterModel(model, object = object)
        x <- CoCoRaw::returnModelNumber("last", object = object)
    }
    else {
        x <- model
        model <- CoCoRaw::returnModel(model, object = object)
        if (!is.number(x)) 
            x <- CoCoRaw::returnModelNumber(x, pop = push.pop, object = object)
    }
    title <- ifelse(title != "", title, model)
    my.assign(".current.coco.model", .new.coco.model(x, model, 
        object = object, title = title), frame = 0)
    CoCoCore::.currentCoCoModel()
}
