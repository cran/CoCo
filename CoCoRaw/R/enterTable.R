"enterTable" <-
function (counts, silent = TRUE, setslot = TRUE, missing = NULL, 
    data = NULL, object = .object.of.thing(data = data, ...), 
    ...) 
{
    "subEnterArray" <- function(data.array, silent = TRUE, setslot = TRUE, 
        missing = NULL, data = NULL, object = .object.of.thing(data = data, 
            ...), ...) {
        Levels <- dim(data.array)
        Names <- names(dimnames(data.array))
        if (is.null(missing)) 
            missing <- rep(0, length(Levels))
        Levels <- Levels - missing
        if (!is.null(Names)) {
            if ((max(nchar(Names)) > 1) && (substr(Names[1][1], 1, 1) != ":"))
                Names <- paste(":", Names, sep = "")
        }
        else {
            n <- dim(data.array)[2]
            if (n < 52) 
                Names <- (.my.ascii()[c(98:123, 66:91)])[1:n]
            else Names <- paste("V", 1:n, sep = "")
        }
        enterNames(names = Names, levels = Levels, missing = missing, 
            object = object)
        enterTable(c(data.array), silent = silent, setslot = setslot, 
            object = object)
    }
    if (is.array(counts)) {
        subEnterArray(counts, silent = silent, setslot = setslot, 
            missing = missing, object = object)
    }
    else {
        if (is.numeric(counts) != TRUE) 
            stop("Counts must be vector of integers")
        if (length(counts) != returnNcells("*", full = TRUE, 
            object = object)) 
            stop("Wrong number of cells in table")
        if (sum(counts[counts < 2147483641]) > 2147483648) 
            stop("Too many observations")
        if ((max(counts) > 2147483640)) 
            stop("Invalid cell count")
        .set.coco.value(object, ".observations", list(type = "table", 
            counts = counts, silent = silent))
        counts[counts == -1] <- 2147483644
        if ((min(counts) < 0)) 
            stop("Invalid cell count")
        result <- call.coco.longs(104, argument = ifelse(counts == -1, 
            2147483644, counts), length = length(counts), sub.code = ifelse(silent, 
            2, 1), object = object)
        cat("\n")
	if (is.null(result))
           return(NULL)
        else
           return(TRUE)
    }
}
