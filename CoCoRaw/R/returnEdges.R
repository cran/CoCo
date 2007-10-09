"returnEdges" <-
function (model = "current", edges = "in.model", fix = FALSE, data = NULL, 
    object = .object.of.model(model, data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    type <- .return.type(object = object)
    if (type == 2) {
        # model <- .recover.model(model)
        # m <- .encode(c("base", "current", "last"), model, -(1:3), -2)
        m <- -2
    }
    else {
        m <- -2
    }
    if (fix == "fix.edges") 
        edges.fix <- 1
    else if (fix == "all") 
        edges.fix <- 0
    else if (fix == "all.edges") 
        edges.fix <- 0
    else if (fix == "ignore.fixing") 
        edges.fix <- 0
    else edges.fix <- -1
    edges.fix <- c(.encode(c("in.model", "all", "all.edges", 
        "not.in.model"), edges, c(1, 0, 0, -1), 1), edges.fix)
    tmp <- call.coco(143, m, arg.long = c(edges.fix, rep(-1, 
        4)), object = object)
    if (70 == tmp$ifail[1]) 
        result <- call.coco(143, m, arg.long = c(edges.fix, rep(-1, 
            tmp$n.args[2] - 2)), object = object)
    else result <- tmp
    result <- .after.set.current(old.current, result, type = "unconditioned", 
        model = FALSE, object = object)
    return(matrix(result$arg.long[result$arg.long >= 0] + 1, 
        ncol = 2, byrow = TRUE))
}
