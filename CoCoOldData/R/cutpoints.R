"cutpoints" <-
function (name, cutpoints, object = CoCoCore::.currentCoCo()) 
{
    if ("what" != name) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "cutpoints", 
            name = name, cutpoints = cutpoints, when = "medio"))
    if ((is.character(name) & is.vector(cutpoints) & is.number(cutpoints[1]))) 
        call.coco.message(101, FALSE, arg.char = name, arg.double = cutpoints, 
            object = object)
    else if ((is.character(name) & ("what" == cutpoints))) {
        result <- call.coco(101, -1, arg.char = name, arg.double = rep(0, 
            32), object = object)
        result$arg.double[1:result$n.arg[3]]
    }
}
