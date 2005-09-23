"returnTable" <-
function (type = "observed", set = "*", model = FALSE, random = FALSE, 
    log.transformed = FALSE, complete = FALSE, discrete.ordered = TRUE, 
    dump = FALSE, label = TRUE, split = TRUE, discrete.outer = TRUE,
    mixed = FALSE, data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    ".return.sparse.table" <- function(type = "sparse.table", 
        set = "*", den = ".", model = FALSE, return.table = TRUE, 
        data = NULL, object = .object.of.model(model, data = data, 
            ...), ...) {
        old.current <- .before.set.current(model, object = object)
        n <- floor(returnTable("observed", ".", object = object)/4)
        size <- floor(numberVariates(object = object)/4)
        .arg.char <- paste(c(set, "/", den, ";"), collapse = "")
        result <- coco.replace.all(code = 114, string = .arg.char, 
            long = rep(0, size * n), double = rep(-1, n), 
	    sub.code = ifelse(return.table, -1, 1), object = object)
        .after.set.current(old.current, result, type = "unconditioned", 
            model = FALSE, object = object)
    }
    ".return.variable.indices" <- function(set = "*", data = NULL, 
        names = .split.name.set(.return.name.list.string(object = object)), 
        object = .object.of.thing(data = data, ...), ...) {
        names.set <- .split.name.set(set)
        match(names.set, names)
    }
    .return.continuous <- function(type = "canonical", set = "*", 
        model = FALSE, random = FALSE, log.transformed = FALSE, 
        complete = FALSE, discrete.ordered = TRUE, dump = FALSE, label = TRUE, 
        split = TRUE, discrete.outer = TRUE, data = NULL, 
	object = .object.of.model(model,  data = data, ...), ...) {
        valid.types <- c("canonical", "moment", "raw", "mk", "ms")
        if (!any(type == valid.types)) 
            stop(paste("Type should be one of: ", paste(valid.types, 
                collapse = ", "), "."))
        .type <- .encode.type.and.options(type, random, log.transformed, 
            complete, discrete.ordered, uniform = FALSE, rankit = FALSE, 
            probit = FALSE)
        if ((16 < .type) && (.type < 32)) 
            .type <- .type - 16
        old.current <- .before.set.current(model, object = object)
        levels <- .return.level.list(full = FALSE, object = object)
        names <- .split.name.set(.return.name.list.string(object = object))
        variable.indices <- .return.variable.indices(set, names = names, 
            object = object)
        if ((type == "ms") || (type == "mk")) 
            if (length(variable.indices) < length(names)) {
                variable.indices <- 1:length(names)
                set <- paste(names, collapse = "")
                warning("Returning model estimates for full variable set!")
            }
        levels <- levels[variable.indices]
        names <- names[variable.indices]
        unknown <- is.na(levels)
        if (any(is.na(variable.indices))) {
            warning("Unrecognized names!")
            names.set <- .split.name.set(set)
            names[unknown] <- names.set[unknown]
        }
        n0 <- length(levels[(levels == 0) & !unknown])
        n1 <- length(levels[unknown])
        n <- n0 + n1
        m <- n + ((n + 1) * n)/2 + 1
        ncells <- returnNcells(set, object = object)
        size <- m * ncells
        result <- call.coco(ifelse(discrete.ordered, 124, 124), 
	    ifelse(dump, 0, -1), arg.char = set, arg.long = .type, 
            arg.double = 1:size, object = object)
        Z <- !(result$arg.double == 1:size) | is.na(result$arg.double)
        if (any(Z)) {
            new.size <- max((1:size)[Z])
            if (size != new.size) {
                result$arg.double <- result$arg.double[1:new.size]
                ncells1 <- prod(levels[(levels != 0) & !unknown])
                levels[unknown] <- 1
                levels[min((1:length(levels))[unknown])] <- ncells%/%ncells1
                n <- n0
                m <- n + ((n + 1) * n)/2 + 1
                size <- m * ncells
                if (size != new.size) {
                  label <- FALSE
                  warning("Unable to guess type of unrecognized names!")
                }
            }
            else levels[unknown] <- 0
            l <- names[levels == 0]
	    names.discrete <- names[levels > 0]
	    levels.discrete <- levels[levels > 0]
            nms <- NULL
	    if (length(levels.discrete) > 0) {
              for (i in 1:length(levels.discrete)) nms <- append(nms, 
                list(paste("", 1:levels.discrete[i], sep = "")))
              names(nms) <- names.discrete
	    }
            x <- rep(1:n, rep(n, n))
            y <- rep(1:n, n)
            labels.B <- NULL
            for (i in 1:length(x)) if (y[i] <= x[i]) 
                labels.B <- c(labels.B, (paste(l[x[i]], " - ", l[y[i]])))
            labels.A <- l
            if (split) {
                X <- result$arg.double
                X <- array(X, c(m, prod(levels[levels != 0])))
                A <- X[1:n, ]
                B <- X[(n + 1):(m - 1), ]
                C <- X[m, ]
                D <- array(dim = c(n, n, dim(B)[2]))
                j <- 1
                if ((ncells > 1) && (length(levels[levels == 0]) > 1)) {
                  for (i in 1:length(x)) if (y[i] <= x[i]) {
                    D[y[i], x[i], ] <- B[j, ]
                    D[x[i], y[i], ] <- B[j, ]
                    j <- j + 1
                  }
                }
                else if (ncells > 1) {
                  D <- B
		  dim(D) <- c(1, 1, length(B))
                }
                else {
                  for (i in 1:length(x)) if (y[i] <= x[i]) {
                    D[y[i], x[i]] <- B[j]
                    D[x[i], y[i]] <- B[j]
                    j <- j + 1
                  }
                }
                if (label) {
                  A <- array(A, c(n, levels[levels != 0]))
                  # dimnames(A) <- rep(list(NULL), length(dim(A)))
		  dimnames(A) <- append(list(NULL), nms)
                  dimnames(A)[[1]] <- labels.A
                  if ((!discrete.outer) && (length(dim(A)) > 1)) 
                    A <- aperm(A, c(2:(length(dim(A))), 1))
                  B <- array(B, c(m - n - 1, levels[levels != 
                    0]))
                  # dimnames(B) <- rep(list(NULL), length(dim(B)))
		  dimnames(B) <- append(list(NULL), nms)
                  dimnames(B)[[1]] <- labels.B
                  if ((!discrete.outer) && (length(dim(B)) > 1)) 
                    B <- aperm(B, c(2:(length(dim(B))), 1))
                  C <- array(C, levels[levels != 0])
                  D <- array(D, c(n, n, levels[levels != 0]))
                  # dimnames(D) <- rep(list(NULL), length(dim(D)))
		  dimnames(D) <- append(list(NULL), append(list(NULL), nms))
                  dimnames(D)[[1]] <- labels.A
                  dimnames(D)[[2]] <- labels.A
                  if ((!discrete.outer) && (length(dim(D)) > 2)) 
                    D <- aperm(D, c(3:(length(dim(D))), 1:2))
                }
                X <- list(A = A, B = B, D = D, C = C)
                if ((type == "canonical") || (type == "mk")) 
                  names(X) <- c("h", "K", "K.t", "G")
                else if (type == "raw") 
                  names(X) <- c("Sum", "SumsOfSquares", "SumsOfSquares.t", 
                    "NaN")
                else if ((type == "moment")) 
                  names(X) <- c("Mean", "SSD", "SSD.t", "Determinant")
                else if ((type == "ms")) 
                  names(X) <- c("Mean", "Variance", "Variance.t", 
                    "N")
                result$arg.double <- X
            }
            else if (label) {
                labels <- c(labels.A, labels.B, "Determinant")
                X <- result$arg.double
                X <- array(X, c(m, levels[levels != 0]))
                # dimnames(X) <- rep(list(NULL), length(dim(X)))
                dimnames(X) <- append(list(NULL), nms)
                dimnames(X)[[1]] <- labels
                if ((!discrete.outer) && (length(dim(X)) > 1)) 
                  X <- aperm(X, c(2:(length(dim(X))), 1))
                result$arg.double <- X
            }
        }
        else result$arg.double <- NULL
        .after.set.current(old.current, result, type = "double", 
            model = FALSE, object = object)
    }
    if (set == "") set <- ";"
    if (set == "*") {
      variableDescription <- returnVariableDescription(object = object);
      set <- paste(variableDescription$names, collapse = "")
    }
    if (type == "sparse.table") {
        result <- .return.sparse.table(type, set = set, den = ".", model = model, 
				       object = object)
	if (label && (length(result) > 1)) {
            names <- .split.name.set(.return.name.list.string(object = object))
            variable.indices <- .return.variable.indices(set, names = names, 
							 object = object)
            if ((length(variable.indices) == 1) && is.na(variable.indices))
              variable.indices <- 1:length(names)
            names <- names[variable.indices]
	    result <- matrix(result[[2]], ncol = length(names) + 1, byrow = T)
	    dimnames(result) <- list(NULL, c("Count", names))
	}
        return(result)
    } else {
        .type <- .encode.type.and.options(type, random, log.transformed, 
            complete, discrete.ordered, uniform = FALSE, rankit = FALSE, 
            probit = FALSE)
        if ((mixed) || ((16 < .type) && (.type < 32))) 
            .return.continuous(type, set = set, model = model, 
                random = random, log.transformed = log.transformed, 
                complete = complete, discrete.ordered = discrete.ordered, 
		dump = dump, label = label, split = split, 
                discrete.outer = discrete.outer, object = object)
        else {
            old.current <- .before.set.current(model, object = object)
            size <- returnNcells(set, object = object)
            result <- call.coco(ifelse(discrete.ordered, 119, 122), ifelse(dump, 
                0, -1), arg.char = set, arg.long = .type, arg.double = 1:size, 
                object = object)
            result <- .after.set.current(old.current, result, 
                type = "double", model = FALSE, object = object)
            if (label && (length(result) > 1)) {
                levels <- .return.level.list(full = FALSE, object = object)
                names <- .split.name.set(.return.name.list.string(object = object))
                variable.indices <- .return.variable.indices(set, 
                  names = names, object = object)
                if ((length(variable.indices) == 1) && is.na(variable.indices)) 
                  variable.indices <- 1:length(levels)
                levels <- levels[variable.indices]
                names <- names[variable.indices]
                result <- array(result, levels)
                nms <- NULL
                for (i in 1:length(levels)) nms <- append(nms, 
                  list(paste("", 1:levels[i], sep = "")))
                names(nms) <- names
                dimnames(result) <- nms
            }
            return(result)
        }
    }
}
