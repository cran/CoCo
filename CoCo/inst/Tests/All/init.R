
source("run.R")
source("run.CoCo.R")

g <- function(data) {
  if (is.data.frame(data)) {
    x <- rep(FALSE, ncol(data))
    for (i in 1:ncol(data)) 
      x[i] <- is.factor(data[,i])
    return(x)
  } else
    return(FALSE)
}

h <- function(data) {
  x <- g(data)
  return(c(length(x[x]), length(x[!x])))
}

f <- function(name, lib, x, IsArray = FALSE, Accumulated = FALSE, ...) { 
  used.time <- rep(0, 5)
  if (Accumulated) {
    dims <- c(dim(x)[2]-1, 0, dim(x)[2]-1, dim(x)[1])
    used.time <- system.time(run.discrete.forward(x, name, 
                                                  accumulated = TRUE, ...))
  } else if (is.array(x) && (IsArray || !is.matrix(x))) {
    S <- sum(x)
    if (is.na(S)) {
      dims <- c(rep(length(dim(x)), 2), 0, -1) 
      dims <- dims[c(1, 3, 2, 4)]
    } else {
      dims <- c(rep(length(dim(x)), 2), 0, S) 
      if ( ((abs(S - round(S)) ) > 0.01) )
        dims <- dims[c(1, 3, 2, 4)] 
      else {
	used.time <- system.time(run.discrete.forward(x, name, ...))
      }
    }
  }
  else if (is.data.frame(x)) {
    dims <- c(dim(x)[2], h(x), dim(x)[1])
    used.time <- system.time(runCg.forward(x, name, ...))
  } else {
    dims <- c(0, 0, 1, length(x))
    if (!is.list(x) && !is.ts(x) && !is.character(x) && !is.null(dim(x))) {
      S <- sum(x)
      if (is.na(S)) {
      } else {
        dims <- c(rep(length(dim(x)), 2), 0, S) 
        if ( ((abs(S - round(S)) ) > 0.01) )
          dims <- dims
        else if (is.matrix(x)) {
	  used.time <- system.time(run.discrete.forward(x, name, 
                                                        accumulated = TRUE, ...))
        }
      }
    }
  }
  print(paste(paste(c("Time(", name, "): " ), collapse = ""), 
              paste(c(format(used.time, nsmall = 2),
                      format(dims, nsmall = 0)), collapse = " ")))
  print(paste(paste(dims, collapse = " "), name))
}
