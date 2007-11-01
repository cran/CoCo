"read.q.list" <-
function (set, setslot = TRUE, object = CoCoCore::.currentCoCo()) 
{
    if (setslot) 
        .set.coco.value(object, ".invalid", append = TRUE, list(type = "read.q.list", 
            set = set, when = "ultimo"))
    coco.enter.string(107, set, FALSE, object = object)
}
