"select.cases" <-
function (set, cell, object = .current.coco) 
{
    if ("what" != set) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "select.cases", 
            set = set, cell = cell, when = "medio"))
    if ((is.character(set) & is.vector(cell) & is.number(cell[1]))) 
        call.coco.message(96, FALSE, arg.char = set, arg.long = cell, 
            object = object)
}
