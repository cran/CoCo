"read.q.table" <-
function (set, setslot = TRUE, object = .current.coco) 
{
    if (setslot) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.q.table", 
            set = set, when = "ultimo"))
    coco.enter.string(106, set, FALSE, object = object)
}
