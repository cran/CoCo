"recover.coco.model" <-
function (coco.model.object, key = .return.key(coco.model.object), 
    model = .return.model.of.object(coco.model.object), level = 90000, 
    pos = .GlobalEnv, new.id = NULL, new.no = NULL) 
{
    ".sub.recover.coco.model" <- function(coco.model.object, 
        name = "", key = .return.key(coco.model.object), 
        model = .return.model.of.object(coco.model.object), 
        level = 1, pos = .GlobalEnv, new.id = NULL, new.no = NULL) {
        .my.trace(".sub.recover.coco.model, start:", level = level, 
            name = name, key = key, model = model, 
        id = .return.reference(coco.model.object))
        coco.object <- NULL
        if (length(new.id) == 0) {
            coco.object <- recover.coco.object(coco.model.object, 
                key = key, level = level + 1000, pos = pos)
            if (is.null(coco.object)) 
                message("NULL coco.object in .sub.recover.coco.model")
            new.id <- .return.reference(coco.object)
        }
        result <- .SetSlotValue(coco.model.object, ".reference", 
            new.id)
        if (length(new.no) == 0) {
            enterModel(model, object = result)
            new.no <- returnModelNumber("last", object = result)
        }
        result <- .SetSlotValue(result, ".model.number", new.no)
        model.id.env <- .find.env(key, number = new.no)
        if (!is.null(model.id.env)) 
            message("Environment already exists in .sub.recover.coco.model!")
        else {
            key.new <- .return.key(object = result)
            if (!(key == key.new)) 
                message("Different keys!!!!")
            if (!is.null(coco.object)) {
                parent.id.env <- .return.id.env(coco.object)
                if (class(coco.object) == "CoCoModelClass") 
                  parent.id.env <- .but.last(parent.id.env)
            }
            else parent.id.env <- .find.env(key)
            parent <- .get.env.CoCoOBJECT(id = parent.id.env)
            env <- .CoCo.toplevel(parent, key = key.new, number = new.no)
            model.id.env <- env$ID
        }
        result <- .SetSlotValue(result, ".id.env", model.id.env)
        my.assign(".current.coco.model", result, frame = 0)
        .my.trace(".sub.recover.coco.model,  stop:", level = level, 
            name = name, key = key, model = .return.model.of.object(result), 
            id = .return.reference(result), 
        number = .return.model.number(result), 
            object = result)
        return(result)
    }
    .my.trace("recover.coco.model,      start:", level = level, 
        name = "xxx", key = key, model = model, 
        id = .return.reference(coco.model.object))
    result <- NULL
    if (.return.reference(coco.model.object) == .endedCoCo()) {
        Objects <- ls(all.names = TRUE, pos = pos)
        for (i in 1:length(Objects)) {
            .object <- get(Objects[i], pos = pos)
            if ((class(.object) == "CoCoModelClass")) 
                if ((.return.key(.object) == key)) 
                  if ((.return.model.of.object(.object) == model)) {
                    if (.return.reference(.object) == .endedCoCo()) {
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
    if (is.null(result) && (class(coco.model.object) == "CoCoClass")) 
        result <- .sub.recover.coco.model(coco.model.object, 
            name = "", key = key, model = model, level = level + 
                100, pos = pos, new.id = new.id, new.no = new.no)
    .my.trace("recover.coco.model,       stop:", level = level, 
        name = "XXX", key = key, model = .return.model.of.object(result), 
        id = .return.reference(result), number = .return.model.number(result), 
        result)
    return(result)
}
