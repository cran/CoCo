
".visitEdges" <-
function(model = "current",
         edges = "in.model",
         fix = FALSE, 
         action = "drop.edges", 
         result.form = "maximal.interaction.terms", 
         section.2.edges = TRUE, 
         make.model = FALSE, 
         return.test = TRUE, 
         push.pop = TRUE, 
         dispose = TRUE, 
         returnList = FALSE, 
         distance = function(object) - (object["deviance"] - object["df"]),
         data = NULL, 
         object = .object.of.model(model, data = data, ...),
         ...) {
    old.current <- .before.set.current(model, object = object)
    # type <- .return.type(object = object)
    variableDescription <- returnVariableDescription(object = object)
    nms <- variableDescription$names
    edgeList <- returnEdges(model = "last",
                            edges = edges, fix = fix, object = object)
    result <- NULL
    modelNumber <- returnModelNumber("last")
    for (i in 1:nrow(edgeList)) {
        edge <- edgeList[i,]
        names(edge) <- c("a", "b")
        modification <- paste(nms[edge], collapse = "")
        print(paste(i, modification))
        r <- editModel(action = action,
                       modification = modification, 
                       model = modelNumber, 
                       result.form = result.form, 
                       omit.test = TRUE, 
                       edges = section.2.edges,
                       make.model = make.model, 
                       return.test = return.test, 
                       push.pop = push.pop, 
                       dispose = FALSE, 
                       object = object)
        if (dispose && r) {
            Model <- returnModel("last", object = object)
            disposeOfModel("last", object = object)
            # r <- c(r, Model)
        }
        Distance <- distance(r)
        names(Distance) <- "distance"
        CardEdge <- length(edge)
        names(CardEdge) <- "c"
        r <- c(edge, CardEdge, r, Distance)
        if (returnList)
          result <- append(result, list(r))
        else
          result <- rbind(result, r)
    }
    EdgeList <- apply(edgeList, 1, function(i) paste(nms[i], collapse = ""))
    if (returnList)
      names(result) <- EdgeList
    else {
      dimnames(result)[[1]] <- EdgeList
      all.missing <- apply(result, 2, function(i) all(is.na(i)))
      result <- result[,!all.missing]
    }
    result <- .after.set.current(old.current, result, type = "unconditioned", 
                                 model = FALSE, object = object)
    return(result)
}

"Backward" <- function(...) .visitEdges(...)
"Forward"  <- function(...) 
              .visitEdges(edges = "not.in.model", action = "add.edges", ...)

".returnStepwise" <-
function (forward = TRUE, sorted = FALSE, separators = FALSE, 
          edges = TRUE, model = FALSE, fix.edges = NULL, 
          distance = function(object) - (object["deviance"] - object["df"]),
          data = NULL, 
          object = .object.of.model(model, data = data, ...), ...) 
{
    type <- .return.type(object)

    if (type == 1) {
       message("Using the slower '.visitEdges' for discrete CoCo object!")
       if (forward)
          Forward(model = model, object = object)
       else
          Backward(model = model, object = object)
    } else {

       old.current <- .before.set.current(model, object = object)
       if (!(is.null(fix.edges))) 
           .fix.edges(fix.edges, object = object)
     
       fix <- !(is.null(fix.edges))
       edgeList <- returnEdges(model = "current", 
                               edges = ifelse(forward, 
                                              "not.in.model", "in.model"),
                               fix = fix, object = object)
       m <- nrow(edgeList)
       names.long <- c("number.of.cases", "df", "adj", "number.of.tables", 
                       "f.test.df", "a", "b", "c")
       names.double <- c("deviance", "e.deviance", "square", "e.square", 
                         "power", "e.power", "gamma", "gamma.s", "gamma.s.1", 
                         "e.gamma.1", "e.gamma.2", 
                         "df.float", "f.test", "e.f.test")
       if (type == 2) {
          arg.long <- rep(0, m * (5+3))
          arg.double <- rep(0, m * 14)
       } else {
          arg.long <- rep(0, m * (4+3))
          arg.double <- rep(0, m * 12)
          names.long <- names.long[-5]
          names.double <- names.double[1:12]
       }
       result <- call.coco(ifelse(forward, 201, 200), 
                           .encode(c("edges", "interactions"), edges, c(1, 2), 
                                   ifelse(edges, 1, 2)) +
                           4 + ifelse(sorted, 0, 8),
                           arg.long = arg.long, 
                           arg.double = arg.double, object = object)

       arg.long <- matrix(result$arg.long, nrow = m, byrow = T)
       dimnames(arg.long) <- list(NULL, names.long)

       arg.double <- matrix(result$arg.double, nrow = m, byrow = T)
       dimnames(arg.double) <- list(NULL, names.double)

       exclude <- (arg.long[, "a"] == 0) &
                  (arg.long[, "b"] == 0) &
                  (arg.long[, "c"] == 0)

       arg.long <- arg.long[!exclude, ]
       arg.double <- arg.double[!exclude, ]

       if (dim(arg.long)[1] > 0) {
         all.missing <- apply(arg.long, 2, function(i) all(is.na(i)))
         arg.long <- arg.long[,!all.missing]
       
         all.missing <- apply(arg.double, 2, function(i) all(is.na(i)))
         arg.double <- arg.double[,!all.missing]
  
         variableDescription <- returnVariableDescription(object = object)
         nms <- variableDescription$names
       
         nnames <- vector("character", nrow(arg.long))
         for (i in 1:nrow(arg.long)) {
           edge <- arg.long[i, c("a", "b")]
           nnames[i] <- paste(nms[edge], collapse = "")
         }
         result <- cbind(arg.long, arg.double)
         dimnames(result)[[1]] <- nnames
  
         distance <- apply(result, 1, function(object) distance(object))
         result <- cbind(result, distance)
       } else result <- NULL
     
       if (old.current) 
           makeCurrent(old.current, object = object)
     
       return(result)
    }
}

"backward" <-
function (sorted = FALSE, reversed = FALSE, only = FALSE, short = FALSE,
    p.accepted = FALSE, p.rejected = FALSE, decomposable.mode = NULL, 
    coherent = FALSE, headlong = FALSE, recursive = FALSE, follow = FALSE, 
    least.significant = TRUE, components = NULL, p.components = FALSE, 
    separators = FALSE, p.separators = FALSE, edges = TRUE, model = FALSE, 
    fix.edges = NULL, 
    return.tests = FALSE,
    data = NULL, object = .object.of.model(model, data = data, ...), ...) 
{
    old.current <- .before.set.current(model, object = object)
    if (!(is.null(fix.edges))) 
        .fix.edges(fix.edges, object = object)
    if (p.components) 
        .set.components(p.components, object = object)
    if (p.separators) 
        .set.separators(p.separators, object = object)
    if (p.accepted) 
        .set.acceptance(p.accepted, object = object)
    if (p.rejected) 
        .set.rejection(p.rejected, object = object)
    if (!is.null(decomposable.mode)) 
        .set.switch("decomposable.mode", ifelse(decomposable.mode, 
            "on", "off"), object = object)
    if (!is.null(components)) 
        .set.switch("partitioning", ifelse(components, 
            "on", "off"), object = object)
    if (only) 
        coco.simple.command(199, 1, object = object)
    if (reversed) 
        coco.simple.command(199, 2, object = object)
    if (sorted) 
        coco.simple.command(199, 3, object = object)
    if (short) 
        coco.simple.command(199, 5, object = object)
    if (recursive) 
        coco.simple.command(199, 10, object = object)
    if (coherent) 
        coco.simple.command(199, 11, object = object)
    if (headlong) 
        coco.simple.command(199, 12, object = object)
    if (follow) 
        coco.simple.command(199, 13, object = object)
    if (separators) 
        coco.simple.command(199, 15, object = object)
    if (least.significant != TRUE) 
        coco.simple.command(199, 20, object = object)
    if (old.current) 
        makeCurrent(old.current, object = object)
    if (return.tests) {
       .returnStepwise(forward = FALSE, sorted = sorted, 
                       separators = separators, edges = edges, 
                       model = model, fix.edges = fix.edges, 
                       object = object, ...) 
    } else {
       result <- coco.simple.command(200, 
                                     ifelse(("edges" == edges), 1, 
                                            ifelse(("interactions" ==  edges),
                                                    2, ifelse(edges, 1, 2))),
                                      object = object)
       if (old.current) 
         makeCurrent(old.current, object = object)
   }

}
