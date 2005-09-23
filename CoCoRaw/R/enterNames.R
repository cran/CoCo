"enterNames" <-
function (names = c("a", "b", "c"), levels = c(2, 2, 2), missing = NULL, 
    setslot = TRUE, data = NULL, object = .object.of.thing(data = data, 
        ...), ...) 
{
    x <- c(49:58, 66:91, 98:123, 96, 34, 38, 64, 65, 59)
    v <- .my.ascii()[x]
    if (length(names) > 1) {
        if ((max(nchar(names)) > 1) && (substr(names[1][1], 1, 1) != ":"))
             names <- paste(":", names, sep = "")
    }
    for (i in 1:length(names)) {
        name <- names[i]
        name <- substring(name, 1:nchar(name), 1:nchar(name))
        j <- match(name, v)
        if (any(is.na(j))) {
            warning(paste("Invalid characters:", paste(name[is.na(j)], 
                collapse = ",")))
            name[is.na(j)] <- "_"
            new.name <- paste(name, collapse = "")
            warning(paste("Replacing ", paste(c(names[i], new.name), 
                collapse = " by ")))
            names[i] <- new.name
        }
    }
    NAMES <- paste(names, collapse = "")
    if (is.character(names) != TRUE) 
        stop("Names must be text-string")
    if (length(NAMES) > 1) 
        stop("One text-string")
    if (length(levels) > 126) 
        warning("To many variables for normal size CoCo")
    if (is.numeric(levels) != TRUE) 
        stop("Levels must be vector of integers")
    if (length(names) > 1) {
        if (length(levels) != length(names)) 
            warning("Diff length of names and levels")
    }
    else if (length(levels) != nchar(names)) 
        if (substr(names, 1, 1) != ":") 
            warning("Diff length of names and levels (if not long names)")
    if (min(levels) < 0) 
        stop("Invalid number of levels")
    if (max(levels) > 250) 
        warning("To many levels for normal size CoCo")
    if (is.numeric(missing) != TRUE & !is.null(missing)) 
        stop("Missing must be vector of integers")
    if (length(levels) < length(missing)) 
        stop("Length of missing greater than length of levels")
    .set.coco.value(object, ".specification", list(type = "names", 
        names = names, levels = levels, missing = missing))
    result <- call.coco(91, 1, arg.char = NAMES, arg.long = c(nchar(NAMES), 
        length(levels), length(missing), levels, missing), object = object)
    ok.coco(result)
}
