"redefine.factor" <-
function (name, levels, missing.levels = 0, object = .current.coco) 
{
    if ("what" != name) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "redefine.factor", 
            name = name, levels = levels, missing = missing, 
            when = "medio"))
    if ((is.character(name) & is.number(levels) & is.number(missing.levels))) 
        call.coco.message(100, FALSE, arg.char = name, arg.long = c(levels, 
            missing.levels), object = object)
}
