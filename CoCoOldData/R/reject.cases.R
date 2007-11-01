"reject.cases" <-
function (set, cell, object = CoCoCore::.currentCoCo()) 
{
    if ("what" != set) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "reject.cases", 
            set = set, cell = cell, when = "medio"))
    if ((is.character(set) & is.vector(cell) & is.number(cell[1]))) 
        call.coco.message(98, FALSE, arg.char = set, arg.long = cell, 
            object = object)
}
