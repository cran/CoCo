"clearCoCoObjects" <-
function (coco.object = NULL, silent = FALSE, pos = .GlobalEnv, 
    printWarnings = FALSE) 
{
    result <- NULL
    graphLattice <- NULL
    if (!is.null(coco.object)) 
        key <- .return.key(coco.object)
    ok <- TRUE
    Objects <- ls(all.names = TRUE, pos = pos)
    if (length(Objects) > 0) 
        for (i in 1:length(Objects)) {
            .object <- get(Objects[i], pos = pos)
            if ((class(.object) == "DynamicGraph")) {
                graphLattice <- c(graphLattice, Objects[i])
            }
            if ((class(.object) == "CoCoModelClass") || (class(.object) == 
                "CoCoClass")) {
                if (!is.null(coco.object)) 
                  ok <- ifelse(.return.key(.object) == key, TRUE, 
                    FALSE)
                if (ok) {
                  result <- c(result, Objects[i])
                  assign(Objects[i], .SetSlotValue(.object, ".reference", 
                    CoCoCore::.endedCoCo()), pos = pos)
                }
            }
            if ((class(.object) == "DynamicGraph")) {
                if (!is.null(coco.object)) 
                  ok <- ifelse(.return.key(.object) == key, TRUE, 
                    FALSE)
                if (ok) {
                  result <- c(result, Objects[i])
                  Frame.Models <- .object
                  for (m in 1:length(Frame.Models@models)) if ((class(Frame.Models@models) == 
                    "CoCoModelClass")) {
                    mdl <- Frame.Models@models[[m]]@model[[1]]
                    mdl <- .SetSlotValue(mdl, ".reference", CoCoCore::.endedCoCo())
                    Frame.Models@models[[m]]@model <- list(mdl)
                  }
                  assign(Objects[i], Frame.Models, pos = pos)
                }
            }
        }
    if ((length(result) > 0) && !silent) {
        message("Warning, ended CoCo-objects: ", paste(result, 
            collapse = ", "), ".")
        if ((length(graphLattice) > 0)) 
            message("Warning, ended DynamicGraph-objects: ", 
                paste(graphLattice, collapse = ", "), ".")
        if (!any(search() == "package:CoCoCg") && 
            !any(search() == "package:CoCo")) {
             # message(paste(
             #           "Please use 'library(CoCo)' and/or 'library(CoCoCg)'",
             #           "before using any CoCo-objects."))
             # message("See 'help(clearCoCoObjects)'.")
        }
    }
    if ((length(graphLattice) > 0) &&
        (as.numeric(version$minor) < 9) && FALSE && !silent) {
        message("Please remove DynamicGraph-objects, quit R by save, and restart R.")
    }
}
