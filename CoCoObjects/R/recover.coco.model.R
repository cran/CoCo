"recover.coco.model" <-
function (coco.model.object, key = .return.key(coco.model.object), 
    model = .return.model.of.object(coco.model.object), level = 90000, 
    pos = .GlobalEnv, new.id = NULL, new.no = NULL) 
{
    .my.trace("recover.coco.model,      start:", level = level, 
        name = "xxx", key = key, model = model, id = .return.reference(coco.model.object))
    if (.return.reference(coco.model.object) == .ended.coco) {
        Objects <- ls(all.names = TRUE, pos = pos)
        for (i in 1:length(Objects)) {
            .object <- get(Objects[i], pos = pos)
            if ((class(.object) == "CoCoModelClass")) 
                if ((.return.key(.object) == key)) 
                  if ((.return.model.of.object(.object) == model)) {
                    if (.return.reference(.object) == .ended.coco) {
                      cat("Recovering CoCo-model: '", Objects[i], 
                        "'.\n")
                      assign(Objects[i], .sub.recover.coco.model(.object, 
                        name = Objects[i], key = key, model = model, 
                        level = level + 100, pos = pos, new.id = new.id, 
                        new.no = new.no), pos = pos)
                    }
                    result <- get(Objects[i], pos = pos)
                    new.id <- .return.reference(result)
                    new.no <- .return.model.number(result)
                  }
        }
    }
    else cat("Model not ended in recovering CoCo-model!!!\n")
    .my.trace("recover.coco.model,       stop:", level = level, 
        name = "XXX", key = key, model = .return.model.of.object(result), 
        id = .return.reference(result), number = .return.model.number(result), 
        result)
    return(result)
}
