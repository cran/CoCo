"sinkCoCo" <-
function (file.name = "Diary.tmp", type = "diary", object = .current.coco) 
{
    if (type == "diary") 
        call.coco.chars(25, file.name, FALSE, object = object)
    else if (type == "report") 
        call.coco.chars(28, file.name, FALSE, object = object)
    else if (type == "log") 
        call.coco.chars(29, file.name, FALSE, object = object)
    else if (type == "dump") 
        call.coco.chars(30, file.name, FALSE, object = object)
    else if (type == "output") 
        call.coco.chars(27, file.name, FALSE, object = object)
}
