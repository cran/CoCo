"returnSets" <-
function (model = FALSE, set = "", set.a = "", set.b = "", u = "", 
    v = "", type = "primes", split.gc = FALSE, split.generators = FALSE, 
    data = NULL, object = .object.of.model(model, data = data, 
        ...), ...) 
{
    ".neighbours" <- function(a, model = FALSE, split.generators = TRUE, 
        data = NULL, object = .object.of.thing(data = data, ...), 
        ...) {
        g <- returnModel(model = model, split.generators = TRUE, 
            object = object)
        if (length(a) == 1) 
            a <- .split.name.set(a)
        b <- sapply(g, function(i) (intersect(a, i)))
        l <- lapply(b, function(i) length(i))
        i <- (1:length(g))[l != 0]
        result <- NULL
        for (j in i) result <- c(result, setdiff(g[[j]], a))
        result <- unique(sort(result))
        if (!split.generators) 
            result <- paste(result, collapse = "")
        return(result)
    }
    old.current <- .before.set.current(model, object = object)
    result <- NULL
    .type <- .return.type(object)
    if (type == "connected.component") {
        result <- coco.replace.all(code = 156, string = paste(c(set, 
            ";"), collapse = ""), sub.code = -1, object = object)
        if ((set != "")) 
            result <- result
    }
    else if (type == "connected.components") 
        result <- coco.replace.all(code = 156, string = paste(c(u, 
            ";"), collapse = ""), sub.code = -2, object = object)
    else if ((type == "prime.components") || (type == "primes")) 
        result <- .return.complex(model = model, omit.separators = TRUE, 
            omit.generators = TRUE, object = object)
    else if ((type == "junction.tree.components") && (.type == 
        2)) 
        result <- .return.complex(model = model, omit.generators = TRUE, 
            object = object)
    else if ((type == "chain.components") && (.type == 2)) 
        result <- coco.enter.string(156, "what", -5, object = object)
    else if ((type == "ancestral.set") && (.type == 2)) 
        result <- coco.enter.string(156, c(set), -6, object = object)
    else if (type == "shortests.paths") 
        result <- coco.replace.all(code = 156, string = paste(c(u, 
            ",", v, ";"), collapse = ""), sub.code = -8, object = object)
    else if ((type == "cut.sets") && (u != "")) 
        result <- coco.replace.all(code = 156, string = paste(c(u, 
            ",", v, ";"), collapse = ""), sub.code = -9, object = object)
    else if (type == "cut.sets") 
        result <- coco.replace.all(code = 156, string = paste(c(set.a, 
            ",", set.b, ";"), collapse = ""), sub.code = -10, 
            object = object)
    else if (type == "separators") 
        result <- .return.complex(model = model, omit.prime.components = TRUE, 
            omit.generators = TRUE, object = object)
    else if ((type == "d-separators") && (.type == 2)) 
        result <- .return.complex(model = model, omit.prime.components = TRUE, 
            omit.generators = TRUE, object = object)
    else if (type == "neighbours") 
        result <- .neighbours(set, model = model, split.generators = split.generators, 
            object = object)
    else if (type == "is.separator") 
        result <- propertySet("separator", set = set, set.a = set.a, 
            set.b = set.b, model = model, object = object)
    else if ((type == "is.d-separator") && (.type == 2)) 
        result <- propertySet("d-separator", set = set, set.a = set.a, 
            set.b = set.b, model = model, object = object)
    if ((type != "is.separator") && (type != "is.d-separator")) {
        result <- .after.set.current(old.current, result, type = "unconditioned", 
            model = FALSE, object = object)
        if (length(result) > 0) 
            if ((type == "junction.tree.components") && (.type == 
                2)) {
                result <- result[[1]]
            }
            else if ((type == "prime.components") || (type == 
                "primes") || (type == "separators")) {
                if (.type == 2) 
                  d <- 0
                else if (type == "separators") 
                  d <- 1
                else d <- 3
                result <- .split.model.gc(result[[1]], split.generators = split.generators, 
                  d = d)
                if (!split.gc) {
                  result <- paste(unlist(result), collapse = "][")
                  result <- list(string = paste(c("[[", result, 
                    "]]"), collapse = ""))
                }
            }
            else if (type == "neighbours") {
                if (split.gc) 
                  result <- list(result)
                else result <- list(string = paste(c("[[", result, 
                  "]]"), collapse = ""))
            }
            else if ((type != "is.separator") && (type != "is.d-separator")) 
                if (split.gc) 
                  result <- .split.model.gc(result[[1]], split.generators = split.generators)
    }
    .end.temporary.object(model, data = data, ...)
    return(result)
}
