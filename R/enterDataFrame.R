"enterDataFrame" <-
function (data.frame, to.factor = NULL, missing.values = c("."), 
    setslot = TRUE, data = NULL, object = .object.of.thing(data = data, ...), 
    ...) 
{
    if (setslot)
        .set.coco.value(object, ".observations", list(type = "mixed", 
            data.frame = data.frame, to.factor = to.factor,
            missing.values = missing.values))
    Names <- dimnames(data.frame)[[2]]
    if (!is.null(Names)) {
        if (max(nchar(Names)) > 1) 
            Names <- paste(":", Names, sep = "")
    }
    else {
        n <- dim(data.frame)[2]
        if (n < 52) 
            Names <- (.my.ascii()[c(98:123, 66:91)])[1:n]
        else Names <- paste("V", 1:n, sep = "")
        dimnames(data.frame)[[2]] <- Names
    }
    if (class(data.frame) == "matrix") {
        warning("Argument data.frame should be of class data.frame, not matrix!")
        data.frame <- data.frame(data.frame)
    }
    Levels <- NULL
    Missing <- NULL
    exclude <- missing.values
    for (i in 1:dim(data.frame)[2]) {
        l <- NULL
        m <- NULL
        if (is.element(i, to.factor)) 
            data.frame[, i] <- factor(data.frame[, i], exclude = NULL)
        else if (is.character(data.frame[, i]) || is.factor(data.frame[, i])) {
            if (is.list(missing.values)) 
                exclude <- missing.values[[i]]
            data.frame[, i] <- factor(ifelse(is.element(data.frame[, 
                i], exclude), NA, data.frame[, i]), exclude = NULL)
        }
        else if (any(is.na(data.frame[, i]))) 
            m <- 1
        a <- levels(data.frame[, i])
        if (!is.null(a)) {
            isna <- is.na(a)
            l <- a[!isna]
            m <- a[isna]
        }
        Levels <- c(Levels, length(l))
        Missing <- c(Missing, length(m))
    }
    enterNames(names = Names, levels = Levels, missing = Missing, 
        object = object)
    .enter.double.list(data.frame, object = object)
}
