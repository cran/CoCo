.packageName <- "CoCoGraph"


require(CoCoObjects)

require(tcltk)
require(dynamicGraph)


".First.lib" <-
function (lib, pkg) 
{
    if (!any(search() == "package:CoCoCg")) 
        require(CoCo)
    # require(tcltk)
    # require(dynamicGraph)
    # .First.lib.CoCoDynamicGraph(lib, pkg)
}
".IsEmpty" <-
function (x) 
{
    if (is.null(x) || (length(x) == 0) || (length(x) == 1) && 
        is.null(x[[1]])) 
        return(TRUE)
    else return(FALSE)
}
".onAttach" <-
function (lib, pkg) 
{
    if (!any(search() == "package:CoCoCg")) 
        require(CoCo)
    # require(tcltk)
    # require(dynamicGraph)
}
".onLoad" <-
function (lib, pkg) 
{
    if (!any(search() == "package:CoCoCg")) 
        require(CoCo)
    # require(tcltk)
    # require(dynamicGraph)
    # .First.lib.CoCoDynamicGraph(lib, pkg)
}

# "newCoCoTestObject" <-
# function (test) 
# {
#     use.ic <- FALSE
#     n.cases <- test["number.of.cases"]
#     df <- test["df"]
#     adj <- test["adj"]
#     adj.df <- df - adj
#     n.tables <- test["number.of.tables"]
#     deviance <- test["deviance"]
#     gamma <- test["gamma"]
#     gamma.s <- test["gamma.s"]
#     e.deviance <- test["e.deviance"]
#     e.gamma.2 <- test["e.gamma.2"]
#     if (use.ic) 
#         if ((!is.numeric(use.ic)) && (is.na(deviance) || is.na(df) || 
#             is.na(adj) || is.na(n.cases) || (n.cases == 0))) 
#             p <- 0
#         else p <- -(deviance - (adj.df * ifelse(is.numeric(use.ic), 
#             use.ic, log(n.cases))))
#     else {
#         if ((!is.na(gamma)) && (-2 < gamma) && (gamma < 2)) {
#             if ((!is.na(e.gamma.2)) && (n.tables > 0) && (e.gamma.2 > 
#                 -1)) 
#                 p <- e.gamma.2
#             else if ((!is.na(gamma.s)) && (gamma.s > 0)) 
#                 p <- 2 * (1 - pnorm((abs(gamma)/sqrt(gamma.s))))
#             else p <- 0
#         }
#         else {
#             if ((!is.na(e.deviance)) && (n.tables > 0) && (e.deviance > 
#                 -1)) 
#                 p <- e.deviance
#             else if (!(is.na(deviance) || is.na(df) || is.na(adj) || 
#                 (adj.df <= 0))) 
#                 p <- (1 - ifelse((adj.df > 0), pchisq(deviance, 
#                   adj.df), 0))
#             else p <- NA
#         }
#     }
#     result <- new("CoCoTestClass", df = df, deviance = deviance, 
#         p = p)
#     return(result)
# }

# "label and "width" (also in ".onLoad.dynamicGraphInterface"), 
#                    has to be exported for edges, nodes, ect.

# "dg": Has to be exported from 'dynamicGraph' for DynamicGraph-class, etc.

# "setSlots": Has to be exported from 'dynamicGraph' for ...

# ("setGraphComponents" and "graphComponents" ("dg.Model"):
#    Exported from 'dynamicGraph' for defining arguments.)

# "setGraphEdges" and "graphEdges" ("dg.Model"):
#    Exported from 'dynamicGraph' for defining arguments.

# "modifyModel" and "testEdge" ("dg.Model"):
#    Now exported from 'dynamicGraph' for defining arguments.

# - thus the stuff with "setGeneric("...", fun)" should not be done
# for the above methods!


# "subModifyModel" is not in 'dynamicGraph'



# ".First.lib.CoCoDynamicGraph" <-
# function (lib, pkg) 
# {

# (Has to be) Exported from 'dynamicGraph':

if (!isGeneric("dg") &&
      (length(attr(isGeneric("dg", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  warning("Method 'dg' should be defined and exported from dynamicGraph")
  if (is.function("dg"))
    fun <- dg
  else
    fun <- function(object, 
                    # modelObject = NULL,
                    # modelObjectName = NULL,
                    # control = dg.control(...), 
                    ...) standardGeneric("dg")
  setGeneric("dg", fun)
}

setMethod("dg", signature(object = "numeric"), 
    function(object, # modelObject = NULL, 
                     # modelObjectName = NULL, 
                     # control = dg.control(...),
                     ...) {
        model <- makeModel(object, ...)
        dg(model, ...)
    })

setMethod("dg", signature(object = "character"), 
    function(object, # modelObject = NULL, 
                     # modelObjectName = NULL, 
                     # control = dg.control(...),
                     ...) {
        model <- makeModel(object, ...)
        dg(model, ...)
    })

if (!isGeneric("setGraphComponents") &&
      (length(attr(isGeneric("setGraphComponents", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  message("Method 'setGraphComponents' set as generic")
  if (is.function("setGraphComponents")) 
      fun <- setGraphComponents
  else fun <- function(object, viewType = NULL, visibleVertices = NULL, 
      visibleBlocks = NULL, extraVertices = NULL, vertexEdges = NULL, 
      blockEdges = NULL, factorVertices = NULL, factorEdges = NULL, 
      extraEdges = NULL, ...) standardGeneric("setGraphComponents")
  setGeneric("setGraphComponents", fun)
}

setMethod("setGraphComponents", signature(object = "CoCoModelClass"), 
    function(object, viewType = NULL, visibleVertices = NULL, 
        visibleBlocks = NULL, extraVertices = NULL, vertexEdges = NULL, 
        blockEdges = NULL, factorVertices = NULL, factorEdges = NULL, 
        extraEdges = NULL, ...) {
        return(object)
    })

if (!isGeneric("recoverModel")) {
  if (is.function("recoverModel")) 
      fun <- recoverModel
  else fun <- function(object, ...) standardGeneric("recoverModel")
  setGeneric("recoverModel", fun)
}

setMethod("recoverModel", signature(object = "CoCoModelClass"), 
    function(object, ...) {
        id.env <- object@.id.env # .return.id.env(object = object)
        env    <- .get.env.CoCoModelOBJECT(id = id.env)
        if (is.null(env) || !(object@.key          == env$env$key)
                         || !(object@.model.number == env$env$number)) {
            message("Hmmm ... seems the model object has to be 'recovered' ... ")
	    object@.reference <- .ended.coco
            object <- .recover.model(object)
        }
        return(object)
    })

if (!isGeneric("setGraphEdges") &&
      (length(attr(isGeneric("setGraphEdges", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  message("Method 'setGraphEdges' set as generic")
  if (is.function("setGraphEdges"))
    fun <- setGraphEdges
  else
    fun <- function(object, dg = NULL, ...)
      standardGeneric("setGraphEdges")
  setGeneric("setGraphEdges", fun)
}

setMethod("setGraphEdges", signature(object = "CoCoModelClass"), 
    function(object, dg = NULL, ...) {
        # if (!is.null(dg)) 
        #   object@dg <- dg
        object <- recoverModel(object, "setGraphEdges")
        return(object)
    })

if (!isGeneric("graphComponents") &&
    (length(attr(isGeneric("graphComponents", getName = TRUE),
                           "package") == "dynamicGraph") > 0)) {
  message("Method 'graphComponents' set as generic")
  if (is.function("graphComponents")) 
      fun <- graphComponents
  else fun <- function(object, viewType = NULL, ...)
                       standardGeneric("graphComponents")
  setGeneric("graphComponents", fun)
}

setMethod("graphComponents", signature(object = "CoCoModelClass"), 
    function(object, viewType = NULL, ...) {

        dots           <- list(...)
        localArguments <-      dots$Arguments

        if (is.null(viewType))
          viewType <- "Simple"

        oriented          <- localArguments$dg@oriented

        edgeColor         <- localArguments$control$edgeColor
        factorVertexColor <- localArguments$control$factorVertexColor
        factorEdgeColor   <- localArguments$control$factorEdgeColor
        blockEdgeColor    <- localArguments$control$blockEdgeColor

        Vertices          <- localArguments$dg@vertexList
        BlockList         <- localArguments$dg@blockList
      # BlockTree         <- localArguments$dg@blockTree
        extraVertices     <- localArguments$dg@extraList

        if (.IsEmpty(extraVertices)) 
            extraVertices <- .emptyDgList("dg.VertexList")
        extraEdges <- localArguments$extraEdgeList
        if (.IsEmpty(extraEdges)) 
            extraEdges <- .emptyDgList("dg.ExtraEdgeList")
        factorClasses <- localArguments$factorClasses
        visibleVertices <- localArguments$visibleVertices
        visibleBlocks <- localArguments$visibleBlocks
        {
            two.to.pairs <- function(from, to) {
              edge.list <- vector("list", length(to))
              for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
                to[j])
              return(edge.list)
            }
            VariableNames <- Names(Vertices)
            type <- .return.type(object)
            model <- returnModel(model = object, split.generators = TRUE)
            Edges <- returnEdges(model = object, fix = "all.edges")
            edge.list <- two.to.pairs(Edges[, 1], Edges[, 2])
            if (type == 2) {
              vV <- returnModelVariates(model = object, as.string = FALSE)
              visibleVertices <- (1:length(vV))[vV == 1]
            }
            else {
              vV <- unique(unlist(model))
              visibleVertices <- match(vV, VariableNames)
            }
            FactorVertices <- .emptyDgList("dg.FactorVertexList")
            FactorEdges <- .emptyDgList("dg.FactorEdgeList")
            if (viewType == "Factor") {
              if (type == 2) {
                f <- function(type) {
                  factors <- returnModel(model = object, 
                    type = type, split.generators = TRUE)
                  lapply(factors, function(i) match(i, VariableNames))
                }
                discrete <- f(type = "discrete")
                linear <- f(type = "linear")
                quadratic <- f(type = "quadratic")
                types <- c(rep("Discrete generator", length(discrete)), 
                  rep("Linear generator", length(linear)), 
                  rep("Quadratic generator", length(quadratic)))
                factors <- append(append(discrete, linear), 
                  quadratic)
              }
              else {
                types <- "Generator"
                factors <- lapply(model, function(i) match(i, 
                  VariableNames))
              }
              if (!(is.null(factors))) {
                result <- returnFactorVerticesAndEdges(Vertices, 
                  factors, types = types, 
                  factorVertexColor = factorVertexColor, 
                  factorEdgeColor = factorEdgeColor, 
                  factorClasses = factorClasses)
                FactorVertices <- result$FactorVertices
                FactorEdges <- result$FactorEdges
                if ((is.null(edge.list))) {
                  from <- result$PairEdges[, 1]
                  to <- result$PairEdges[, 2]
                  edge.list <- two.to.pairs(from, to)
                }
              }
            }
            edgeList <- returnEdgeList(edge.list, Vertices, 
              color = edgeColor, oriented = oriented)
            BlockEdges <- .emptyDgList("dg.BlockEdgeList")
            visibleBlocks <- NULL
            if (((!.IsEmpty(BlockList) && (length(BlockList) > 0)) 
               # || (!is.null(BlockTree) && (length(BlockTree) > 0)
               # && !is.null(BlockTree[[1]]))
               )) {
              message("Blocks not tested!")
              if (!.IsEmpty(FactorVertices)) 
                message("Edges between blocks and factors not implemented!")
              # if (.IsEmpty(BlockList) && !is.null(BlockTree)) 
              #   BlockList <- blockTreeToList(BlockTree)
              visibleBlocks <- 1:length(BlockList)
              BlockEdges <- returnBlockEdgeList(edge.list, 
                Vertices, BlockList, color = blockEdgeColor, 
                oriented = oriented)
            }
        }
        if (viewType == "Factor") 
            list(vertexEdges = edgeList, 
              blockEdges = BlockEdges, 
              factorVertices = FactorVertices, 
              factorEdges = FactorEdges, 
              visibleVertices = visibleVertices, 
              visibleBlocks = visibleBlocks, 
              extraVertices = extraVertices)
        else list(vertexEdges = edgeList, 
            blockEdges = BlockEdges, 
            FactorVertices = .emptyDgList("dg.FactorVertexList"), 
            FactorEdges = .emptyDgList("dg.FactorEdgeList"), 
            visibleVertices = visibleVertices, 
            visibleBlocks = visibleBlocks, 
            extraVertices = extraVertices, 
            extraEdges = extraEdges)
    })

if (!isGeneric("graphEdges") &&
      (length(attr(isGeneric("graphEdges", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  message("Method 'graphEdges' set as generic")
  if (is.function("graphEdges"))
    fun <- graphEdges
  else
    fun <- function(object, viewType = NULL, ...)
    standardGeneric("graphEdges")
  setGeneric("graphEdges", fun)
}

setMethod("graphEdges", signature(object = "CoCoModelClass"), 
    function(object, viewType = NULL, ...) {

        two.to.pairs <- function(from, to) {
          edge.list <- vector("list", length(to))
          for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
            to[j])
          return(edge.list)
        }

        dots           <- list(...)

        localArguments <-      dots$Arguments

        object         <- recoverModel(object, "graphEdges")

        returnVertices <- FALSE
        if (!is.null(dots$returnVertices))
          returnVertices <- dots$returnVertices

        if (is.null(viewType))
          viewType <- "Simple"

        control <- dots$control
        if (is.null(control) && !is.null(localArguments))
          control <- localArguments$control
        if (is.null(control))
          control <- dg.control()

        dg <- dots$dg
        if (is.null(dg) && !is.null(localArguments))
          dg <- localArguments$dg

        edgeColor         <- control$edgeColor
        factorVertexColor <- control$factorVertexColor
        factorEdgeColor   <- control$factorEdgeColor
        blockEdgeColor    <- control$blockEdgeColor

        factorClasses     <- control$factorClasses

        if (!is.null(dg)) {
          oriented      <- dg@oriented
          Vertices      <- dg@vertexList
          VariableNames <- Names(Vertices)
        } else {
          oriented      <- NA
          VariableDescription <- 
            returnVariableDescription(object = object, levels = FALSE)
          VariableNames <- VariableDescription$names
        }

        type <- .return.type(object)
        model <- returnModel(model = object, split.generators = TRUE)
        Edges <- returnEdges(model = object, fix = "all.edges")
        edge.list <- two.to.pairs(Edges[, 1], Edges[, 2])

        if (type == 2) {
          vV <- returnModelVariates(model = object, as.string = FALSE)
          visibleVertices <- (1:length(vV))[vV == 1]
        }
        else {
          vV <- unique(unlist(model))
          visibleVertices <- match(vV, VariableNames)
        }

        if (is.null(dg)) {

          Types <- VariableDescription$types
          types <- validVertexClasses()[, 1][ifelse(Types == 0, 3, Types)]

          sg <- new("dg.simple.graph", 
                    vertex.names = VariableNames, types = types, 
                    from = Edges[, 1], to = Edges[, 2])

          # dg <- as("dg.graph", sg)

          dg <- simpleGraphToGraph(sg, ...)

          Vertices          <- dg@vertexList

        }

        BlockList         <- dg@blockList
      # BlockTree         <- dg@blockTree
        extraVertices     <- dg@extraList

     # if (.IsEmpty(extraVertices)) 
     #    extraVertices   <- .emptyDgList("dg.VertexList")
        extraEdges        <- dg@extraEdgeList
     #  if (.IsEmpty(extraEdges)) 
     #    extraEdges      <- .emptyDgList("dg.ExtraEdgeList")
        visibleVertices   <- dg@visibleVertices
        visibleBlocks     <- dg@visibleBlocks

        {

            edgeList <- returnEdgeList(edge.list, Vertices, 
              color = edgeColor, oriented = oriented)

            FactorVertices <- .emptyDgList("dg.FactorVertexList")
            FactorEdges <- .emptyDgList("dg.FactorEdgeList")

            if (viewType == "Factor") {
              if (type == 2) {
                f <- function(type) {
                  factors <- returnModel(model = object, 
                    type = type, split.generators = TRUE)
                  lapply(factors, function(i) match(i, VariableNames))
                }
                discrete <- f(type = "discrete")
                linear <- f(type = "linear")
                quadratic <- f(type = "quadratic")
                types <- c(rep("Discrete generator", length(discrete)), 
                  rep("Linear generator", length(linear)), 
                  rep("Quadratic generator", length(quadratic)))
                factors <- append(append(discrete, linear), 
                  quadratic)
              }
              else {
                types <- "Generator"
                factors <- lapply(model, function(i) match(i, 
                  VariableNames))
              }
              if (!(is.null(factors))) {
                result <- returnFactorVerticesAndEdges(Vertices, 
                  factors, types = types, 
                  factorVertexColor = factorVertexColor, 
                  factorEdgeColor = factorEdgeColor, 
                  factorClasses = factorClasses)
                FactorVertices <- result$FactorVertices
                FactorEdges <- result$FactorEdges
                if ((is.null(edge.list))) {
                  from <- result$PairEdges[, 1]
                  to <- result$PairEdges[, 2]
                  edge.list <- two.to.pairs(from, to)
                }
              }
            }

            BlockEdges <- .emptyDgList("dg.BlockEdgeList")
            visibleBlocks <- numeric(0)
            if (((!.IsEmpty(BlockList) && (length(BlockList) > 0)) 
               # || (!is.null(BlockTree) && (length(BlockTree) > 0)
               # && !is.null(BlockTree[[1]]))
               )) {
              message("Blocks not tested!")
              if (!.IsEmpty(FactorVertices)) 
                message("Edges between blocks and factors not implemented!")
              # if (.IsEmpty(BlockList) && !is.null(BlockTree)) 
              #   BlockList <- blockTreeToList(BlockTree)
              visibleBlocks <- 1:length(BlockList)
              BlockEdges <- returnBlockEdgeList(edge.list, 
                Vertices, BlockList, color = blockEdgeColor, 
                oriented = oriented)
            }

        }

        if (returnVertices) 
          new("dg.graph", 
               viewType         = viewType, 
               vertexList       = Vertices,
               oriented         = oriented, 
               edgeList         = edgeList, 
               blockEdgeList    = BlockEdges, 
               factorVertexList = FactorVertices,
               factorEdgeList   = FactorEdges,
               visibleVertices  = visibleVertices, 
               visibleBlocks    = visibleBlocks, 
               extraList        = extraVertices, 
               extraEdgeList    = extraEdges)
        else
          new("dg.graphedges", 
               viewType         = viewType, 
               oriented         = oriented, 
               edgeList         = edgeList, 
               blockEdgeList    = BlockEdges, 
               factorVertexList = FactorVertices,
               factorEdgeList   = FactorEdges,
               visibleVertices  = visibleVertices, 
               visibleBlocks    = visibleBlocks, 
               extraList        = extraVertices, 
               extraEdgeList    = extraEdges)
    })

# setMethod("vertexEdges", signature(object = "CoCoModelClass"), 
#     function(object) NULL)

OldCoCoDrawModel <- function(object, slave = FALSE, viewType = "Simple", ...) {

    args <- list(...)
    Args <- args$Arguments

    if (class(object) == "CoCoModelClass") 
      Object <- object
    else Object <- makeModel(object)

    title <- Object@.title

    Edges <- graphComponents(Object, viewType, Arguments = Args)
    edgeList <- Edges$vertexEdges
    FactorVertices <- Edges$factorVertices
    FactorEdges <- Edges$factorEdges
    BlockEdges <- Edges$blockEdges
    visualVertices <- Edges$visualVertices
    if (slave) {
      DynamicGraph(addModel = TRUE, frameModels = Args$frameModels, 
        frameViews = NULL, graphWindow = NULL, edgeList = edgeList, 
        object = Object, factorVertexList = FactorVertices, 
        factorEdgeList = FactorEdges, blockEdgeList = BlockEdges, 
        title = title, Arguments = Args)
    }
    else {
      DynamicGraph(overwrite = TRUE, addModel = TRUE, 
        frameModels = Args$frameModels, frameViews = Args$frameViews, 
        graphWindow = Args$graphWindow, edgeList = edgeList, 
        object = Object, factorVertexList = FactorVertices, 
        factorEdgeList = FactorEdges, blockEdgeList = BlockEdges, 
        title = "Not used!", width = NULL, height = NULL, 
        Arguments = Args)
    }
}

CoCoDrawModel <- function(object, slave = FALSE, viewType = "Simple", ...) {

    dots <- list(...)
    localArguments    <- dots$Arguments

    # Here you should make your new model.
    # A copy is made by the following:

    if (class(object) == "CoCoModelClass") 
      ModelObject <- object
    else 
      ModelObject <- makeModel(object)

    title <- ModelObject@.title

    # and compute graph edges:

    dgEdges <- graphEdges(ModelObject, viewType, Arguments = localArguments)

    show(dgEdges)

    if (slave) {
      # Drawing ''an other model'' in a new window:
      addModel(dgEdges, 
               frameModels =        localArguments$frameModels, 
             # control =            localArguments$control, 
               modelObject =        ModelObject, 
               modelObjectName =    title)
    }
    else {
      # Overwriting with ''an other model'' in same view:
      replaceModel(dgEdges,
                   frameModels     = localArguments$frameModels, 
                   modelIndex      = localArguments$modelIndex, 
                   viewIndex       = localArguments$viewIndex, 
                 # control         = localArguments$control, 
                   modelObject     = ModelObject, 
                   modelObjectName = title) 
    }
}

CoCoLabelAllEdges <- function(object, slave = FALSE,  ...) {

  dots           <- list(...)
  localArguments <-      dots$Arguments
  dg             <-      localArguments$dg

  getNodeName <- function(index, type)
    if (type == "Vertex")
      name(localArguments$frameModel@vertices[[index]])
    else if (type == "Factor")
      name(localArguments$dg@factorVertexList[[abs(index)]])
    else if (type == "Extra")
      name(localArguments$dg@extraList[[abs(index)]])
    else if (type == "Block")
      label(localArguments$dg@blockList[[abs(index)]])
    else
      NULL

  visitEdges <- function(edges) {
    for (i in seq(along = edges)) {
      vertices <- nodeIndicesOfEdge(edges[[i]])
      types    <- nodeTypesOfEdge(edges[[i]])

      name.f <- getNodeName(vertices[1], types[1])
      name.t <- getNodeName(vertices[2], types[2])

      R <- testEdge(object, action = "remove",
                    name.1 = name.f, name.2 = name.t,
                    from = vertices[1], to = vertices[2],
                    from.type = types[1], to.type = types[2],
                    edge.index = i, force = force, Arguments = localArguments)

      if (!is.null(R)) {
        if (TRUE || (hasMethod("label", class(R))))
          label(edges[[i]]) <- label(R)
        if (TRUE || (hasMethod("width", class(R))))
          width(edges[[i]]) <- width(R)
      }
    }
    return(edges)
  }

  dg@edgeList       <- visitEdges(dg@edgeList)
  dg@factorEdgeList <- visitEdges(dg@factorEdgeList)
  dg@blockEdgeList  <- visitEdges(dg@blockEdgeList)

  if (slave) {
    # Drawing ''an other model'' in a new window:
    addModel(dg, 
             frameModels     = localArguments$frameModels, 
           # control         = localArguments$control, 
             modelObject     = localArguments$object, 
             modelObjectName = localArguments$object@name)
  }
  else {
    # Overwriting with ''an other model'' in same view:
    replaceModel(dg,
                 frameModels     = localArguments$frameModels, 
                 frameViews      = localArguments$frameViews, 
                 graphWindow     = localArguments$graphWindow, 
               # control         = localArguments$control, 
                 modelObject     = localArguments$object, 
                 modelObjectName = localArguments$object@name)
  }

}

cmdPositions <- function(object, N = NULL, doIso = FALSE,  ...) {
  localArguments <- list(...)$Arguments
  frameModels    <- localArguments$frameModel
  Vertices       <- frameModels@vertices
  Edges          <- localArguments$dg@edgeList
  positions      <- Positions(Vertices)
  N.0 <- dim(positions)[2]
  if (is.null(N)) 
    N <- N.0
  e <- NodeIndices(Edges)
  n <- Names(Vertices)
  X <- matrix(rep(-1, length(n)^2), ncol = length(n))
  for (i in 1:length(e)) {
    suppressWarnings(w <- as.numeric(names(e)[i]))
    if (is.na(w)) 
      w <- 0.5
    X[e[[i]][1], e[[i]][2]] <- w
    X[e[[i]][2], e[[i]][1]] <- w
  }
  dimnames(X) <- list(n, n)
  d <- 1.25
  X[X == -1] <- d
  X <- X - d * diag(length(n))
  if (doIso) {
    require(MASS)
    X[X <= 0] <- 0.001
    mdsX <- isoMDS(X, k = N)
  }
  else mdsX <- cmdscale(X, k = N, add = TRUE, 
                        eig = TRUE, x.ret = TRUE)
  M <- max(abs(mdsX$points))
  new.positions <- mdsX$points / M * 45
  if (N < N.0)
    for (i in N.0 - N)
      new.positions <- cbind(new.positions, 0)
  Positions(Vertices) <- new.positions
  vertices(frameModels) <- Vertices
}

CoCoMenu <- function() 
  list(MainUser = list(
         label = "Position of \"vertices\" by 'myCmdPositions', and redraw", 
         command = function(object, ...) 
           myCmdPositions(object, ...)), 
       MainUser = list(
         label = "Position of \"vertices\" by 'cmdscale', and redraw", 
         command = function(object, ...) 
           cmdPositions(object, ...)), 
       MainUser = list(
         label = "Position of \"vertices\" by 'isoMDS', k = 2, and redraw", 
         command = function(object, ...) 
           cmdPositions(object, N = 2, doIso = TRUE, ...)), 
       MainUser = list(
         label = "Position of \"vertices\"", 
         command = function(object, ...) 
           print(Positions(list(...)$Arguments$vertexList))), 
       MainUser = list(
         label = "Label all edges, in this window", 
         command = function(object, ...) 
           CoCoLabelAllEdges(object, slave = FALSE, ...)), 
       MainUser = list(
         label = "Label all edges, in slave window", 
         command = function(object, ...) 
           CoCoLabelAllEdges(object, slave = TRUE, ...)), 
       MainUser = list(
         label = "Draw model, in this window", 
         command = function(object, ...) {
           Args <- list(...)$Arguments
           ReturnVal <- modalDialog("Model entry modalDialog", 
             "Enter number or tag", "last", top = Args$top)
           if (ReturnVal == "ID_CANCEL") return()
           model <- suppressWarnings(as.numeric(ReturnVal))
           if (is.na(model)) model <- ReturnVal
           CoCoDrawModel(object = model, slave = FALSE, ...)
         }), 
       MainUser = list(
         label = "Draw model, in slave window", 
         command = function(object, ...) {
           Args <- list(...)$Arguments
           ReturnVal <- modalDialog("Model entry modalDialog", 
             "Enter number or tag", "last", top = Args$top)
           if (ReturnVal == "ID_CANCEL") return()
           model <- suppressWarnings(as.numeric(ReturnVal))
           if (is.na(model)) model <- ReturnVal
           CoCoDrawModel(object = model, slave = TRUE, ...)
         }), 
       Vertex = list(
         label = "Test of user popup menu for vertices", 
         command = function(object, name, ...) {
           print(name)
           print(c(list(...)$index))
         }), 
       Edge = list(
         label = "Test of user popup menu for edges", 
         command = function(object, name1, name2, ...) {
           args <- list(...)
           print(c(name1, name2))
           print(c(args$edge.index, args$from, args$to))
         }), 
       ClosedBlock = 
         list(label = "Test of user popup menu for blocks", 
         command = function(object, name, ...) {
           print(name)
           print(c(list(...)$index))
         }))

setMethod("dg", signature(object = "CoCoModelClass"), 
          function(object, # modelObject = NULL, 
                           # modelObjectName = NULL, 
                           # control = dg.control(...),
                           ...) {
      if (TRUE) {
        dots <- list(...)
        if (any(names(args) == "dynamicGraph")) {
            doAdd <- TRUE
            message("use 'addModel', 'addView', ...")
        }
        object <- recoverModel(object, "dg")
        control <- dots$control
        localArguments <- dots$Arguments
        if (is.null(control) && !is.null(localArguments))
          control <- localArguments$control
        if (is.null(control))
          control <- dg.control(...)
        control$UserMenus <- append(CoCoMenu(), control$UserMenus)
        graph <- graphEdges(object, returnVertices = TRUE, ...)
        dg(graph, modelObject = object, modelObjectName = NULL, 
                  control = control)
      } else {
        Edges <- returnEdges(model = object, fix = "all.edges")
        VariableDescription <- returnVariableDescription(object = object, 
            levels = FALSE)
        if (.return.type(object) == 2) {
            vV <- returnModelVariates(model = object, as.string = FALSE)
            visibleVertices <- (1:length(vV))[vV == 1]
        }
        else {
            VariableNames <- VariableDescription$names
            model <- returnModel(model = object, split.generators = TRUE)
            vV <- unique(unlist(model))
            visibleVertices <- match(vV, VariableNames)
        }
        args <- list(...)
        doAdd <- FALSE
        if (any(names(args) == "dynamicGraph")) {
            doAdd <- TRUE
            linkDynamicGraph <- args$dynamicGraph
        }
        if (doAdd) {
            if (is.null(list(...)$UserMenus)) 
              UM <- CoCoMenu()
            else UM <- list(...)$UserMenus
            DynamicGraph(addModel = TRUE, frameModels = linkDynamicGraph, 
              visibleVertices = visibleVertices, from = Edges[, 
                1], to = Edges[, 2], object = object, UserMenus = UM, 
              ...)
        }
        else {
            Types <- VariableDescription$types
            types <- validVertexClasses()[, 1][ifelse(Types == 0, 3, Types)]
            if (isClass("dg.Node")) {
              if (is.null(list(...)$UserMenus)) 
                DynamicGraph(names = VariableDescription$names, 
                  visibleVertices = visibleVertices, types = types, 
                  from = Edges[, 1], to = Edges[, 2], object = object, 
                  UserMenus = CoCoMenu, ...)
              else DynamicGraph(names = VariableDescription$names, 
                visibleVertices = visibleVertices, types = types, 
                from = Edges[, 1], to = Edges[, 2], object = object, 
                ...)
            }
            else {
              warning("Remove objects of class 'DynamicGraph' and restart R.")
            }
        }
      }
    })

setClass("CoCoTestClass", representation(deviance = "numeric", 
    df = "numeric", p = "numeric"))

# (Has to be) Exported from 'dynamicGraph':

if (!isGeneric("label") &&
      (length(attr(isGeneric("label", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  warning("Method 'label' should be defined and exported from dynamicGraph")
  if (is.function("label"))
    fun <- label
  else
    fun <- function(object) standardGeneric("label")
  setGeneric("label", fun)
}

setMethod("label", "CoCoTestClass", 
    function(object) format(object@p, digits = 4))

# (Has to be) Exported from 'dynamicGraph':

if (!isGeneric("width") &&
      (length(attr(isGeneric("width", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  warning("Method 'width' should be defined and exported from dynamicGraph")
  if (is.function("width"))
    fun <- width
  else
    fun <- function(object) standardGeneric("width")
  setGeneric("width", fun)
}

setMethod("width", "CoCoTestClass", 
    function(object) round(2 + 5 * (1 - object@p)))

# Now exported from 'dynamicGraph':

if (!isGeneric("testEdge") &&
      (length(attr(isGeneric("testEdge", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  message("Method 'testEdge' set as generic")
  if (is.function("testEdge"))
    fun <- testEdge
  else
    fun <- function(object, action, name.1, name.2, ...) 
           standardGeneric("testEdge")
  setGeneric("testEdge", fun)
}

setMethod("testEdge", signature(object = "CoCoModelClass"), 
    function(object, action, name.1, name.2, ...) {
        args <- list(...)
        Args <- args$Arguments
        from.type <- args$from.type
        to.type <- args$to.type
        f <- function(type) if (is.null(type)) 
            ""
        else paste("(", type, ")")
        if (!.IsEmpty(args$Arguments$blockList) ||
            (!is.null(args$Arguments$oriented) && 
            !is.na(args$Arguments$oriented) && args$Arguments$oriented)) {
            message <- paste("Test of the edge from", name.1, 
              "to", name.2, " is not implemented for causal models!!!")
            message(message)
            warning(message)
        }
        objectModel <- CoCoObjects::.recover.model(object)
        if (FALSE) {
            new.model <- subModifyModel(objectModel,
              action = "drop.edges", 
              modification = paste(name.1, name.2, sep = ""), 
              ...)
            test <- CoCoRaw::returnTest(model.1 = new.model@.model.number, 
              model.2 = objectModel@.model.number, push.pop = TRUE, 
              object = object)
        }
        else {
            test <- subModifyModel(objectModel, action = "drop.edges", 
              make.model = FALSE, return.test = TRUE, push.pop = TRUE, 
              modification = paste(name.1, name.2, sep = ""), 
              ...)
        }
        return(new("CoCoTestClass", test = test))
    })

# Not in 'dynamicGraph':

if (!isGeneric("subModifyModel")) {
  if (is.function("subModifyModel")) 
      fun <- subModifyModel
  else fun <- function(object, action = NULL, modification = NULL, 
      result.form = "maximal.interaction.terms", section.2.edges = TRUE, 
      make.model = TRUE, return.test = FALSE, push.pop = TRUE, 
      dispose = FALSE, ...) standardGeneric("subModifyModel")
  setGeneric("subModifyModel", fun)
}

setMethod("subModifyModel", signature(object = "CoCoModelClass"), 
    function(object, action = NULL, modification = NULL, 
        result.form = "maximal.interaction.terms", section.2.edges = TRUE, 
        make.model = TRUE, return.test = FALSE, push.pop = TRUE, 
        dispose = FALSE, ...) {
        args <- list(...)
        object <- recoverModel(object, "subModifyModel")
        result <- CoCoRaw::editModel(action = action,
            modification = modification, 
            model = object@.model.number, result.form = result.form, 
            omit.test = TRUE, edges = section.2.edges,
            make.model = make.model, 
            return.test = return.test, push.pop = push.pop, 
            dispose = dispose, object = object)
        return(result)
    })

# Now exported from 'dynamicGraph':

if (!isGeneric("modifyModel") &&
      (length(attr(isGeneric("modifyModel", getName = TRUE),
                             "package") == "dynamicGraph") > 0)) {
  message("Method 'modifyModel' set as generic")
  if (is.function("modifyModel"))
    fun <- modifyModel
  else
    fun <- function(object, action, name, name.1, name.2, ...)
                    standardGeneric("modifyModel")
  setGeneric("modifyModel", fun)
}

setMethod("modifyModel", signature(object = "CoCoModelClass"), 
    function(object, action, name, name.1, name.2, ...) {
        args <- list(...)
        Arguments <- args$Arguments
        FactorVertices <- .emptyDgList("dg.FactorVertexList")
        FactorEdges <- .emptyDgList("dg.FactorEdgeList")
        if (!.IsEmpty(Arguments$blockList)) 
            warning("Interface for Block-recursive models not implemented!!!")
        f <- function(type) if (is.null(type)) 
            ""
        else paste("(", type, ")")
        if (action == "dropEdge") {
            new.object <- subModifyModel(object, action = "drop.edges", 
              modification = paste(name.1, name.2, sep = ""), 
              ...)
        }
        else if (action == "addEdge") {
            new.object <- subModifyModel(object, action = "add.edges", 
              modification = paste(name.1, name.2, sep = ""), 
              ...)
        }
        else if (action == "dropVertex") {
            if (!is.null(Arguments) && (args$index > 0) && 
                !.IsEmpty(Arguments$factorVertexList) &&
                !.IsEmpty(Arguments$vertexList)) {
              x <- (Arguments$factorVertexList)
              factors <- lapply(x, function(i) i@vertex.indices)
              types <- lapply(x, function(i) class(i))
              factors <- lapply(factors, function(x) x[x != 
                args$index])
              if (!(is.null(factors))) {
                result <- returnFactorVerticesAndEdges(
                  Arguments$vertexList, 
                  factors, types)
                FactorVertices <- result$FactorVertices
                FactorEdges <- result$FactorEdges
              }
            }
            new.object <- subModifyModel(object, action = "drop.factor", 
              modification = name, ...)
        }
        else if (action == "addVertex") {
            new.object <- subModifyModel(object, 
              action = "add.interactions", 
              modification = name, ...)
        }
        if ((Arguments$viewType == "Factor") && .IsEmpty(FactorVertices)) {
            graphComponents <- graphComponents(new.object, 
              viewType = Arguments$viewType, Arguments = Arguments)
            FactorVertices <- graphComponents$factorVertices
            FactorEdges <- graphComponents$factorEdges
        }
        result <- list(object = new.object, 
            FactorVertices = FactorVertices, 
            FactorEdges = FactorEdges)
        return(result)
    })

# } # ".First.lib.CoCoDynamicGraph" 

setMethod("setSlots", "CoCoTestClass", function(object, arguments) {
    for (i in seq(along = arguments)) {
        name <- names(arguments)[i]
        if (is.element(name, slotNames(object))) 
            slot(object, name) <- arguments[[i]]
        else message(paste("Argument '", name, "' not valid slot of '", 
            class(object), "', thus ignored.", sep = ""))
    }
    return(object)
})

setMethod("initialize", "CoCoTestClass", function(.Object, ...) {
    Args <- list(...)
    test <- Args$test
    use.ic <- FALSE
    n.cases <- test["number.of.cases"]
    df <- test["df"]
    adj <- test["adj"]
    adj.df <- df - adj
    n.tables <- test["number.of.tables"]
    deviance <- test["deviance"]
    gamma <- test["gamma"]
    gamma.s <- test["gamma.s"]
    e.deviance <- test["e.deviance"]
    e.gamma.2 <- test["e.gamma.2"]
    if (use.ic) 
        if ((!is.numeric(use.ic)) && (is.na(deviance) || is.na(df) || 
            is.na(adj) || is.na(n.cases) || (n.cases == 0))) 
            p <- 0
        else p <- -(deviance - (adj.df * ifelse(is.numeric(use.ic), 
            use.ic, log(n.cases))))
    else {
        if ((!is.na(gamma)) && (-2 < gamma) && (gamma < 2)) {
            if ((!is.na(e.gamma.2)) && (n.tables > 0) && (e.gamma.2 > -1)) 
                p <- e.gamma.2
            else if ((!is.na(gamma.s)) && (gamma.s > 0)) 
                p <- 2 * (1 - pnorm((abs(gamma)/sqrt(gamma.s))))
            else p <- 0
        }
        else {
            if ((!is.na(e.deviance)) && (n.tables > 0) && (e.deviance > -1)) 
                p <- e.deviance
            else if (!(is.na(deviance) || is.na(df) || is.na(adj) || 
                (adj.df <= 0))) 
                p <- (1 - ifelse((adj.df > 0), pchisq(deviance, 
                  adj.df), 0))
            else p <- NA
        }
    }
    result <- list(df = df, deviance = deviance, p = p)
    Args <- append(Args[!names(Args) == "test"], result)
    .Object <- setSlots(.Object, Args)
    return(.Object)
})
