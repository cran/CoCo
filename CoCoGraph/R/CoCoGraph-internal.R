".First.lib" <-
function (lib, pkg) 
{
    if (!any(search() == "package:CoCoCg"))
       require(CoCo)
    # require(CoCoObjects)
    require(tcltk)
    require(dynamicGraph)
    .First.lib.CoCoDynamicGraph(lib, pkg)
}
".First.lib.CoCoDynamicGraph" <-
function (lib, pkg) 
{
    # require(CoCoObjects)
    if (!isGeneric("dynamic.Graph")) {
        if (is.function("dynamic.Graph")) 
            fun <- dynamic.Graph
        else fun <- function(object, ...) standardGeneric("dynamic.Graph")
        setGeneric("dynamic.Graph", fun)
    }
    setMethod("dynamic.Graph", signature(object = "numeric"), 
        function(object, ...) {
            model <- makeModel(object, ...)
            dynamic.Graph(model, ...)
        })
    setMethod("dynamic.Graph", signature(object = "character"), 
        function(object, ...) {
            model <- makeModel(object, ...)
            dynamic.Graph(model, ...)
        })
    setMethod("dynamic.Graph", signature(object = "CoCoModelClass"), 
        function(object, ...) {
            CoCoDrawModel <- function(object, slave = FALSE, 
                oriented = FALSE, edgeColor = "black", 
                factorVertexColor = "default", factorEdgeColor = "brown", 
                blockEdgeColor = "default", ...) {
                two.to.pairs <- function(from, to) {
                  edge.list <- vector("list", length(to))
                  for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
                    to[j])
                  return(edge.list)
                }
                args <- list(...)
                Args <- args$Arguments
                Vertices <- Args$vertexList
                Edges <- returnEdges(model = object, fix = "all.edges")
                edge.list <- two.to.pairs(Edges[, 1], Edges[, 
                  2])
                Object <- makeModel(object)
                title <- Object@.title
                factors <- NULL
                FactorVertices <- NULL
                FactorEdges <- NULL
                if (!(is.null(factors))) {
                  message("Not tested!")
                  result <- returnFactorVerticesAndEdges(Vertices, 
                    factors, factorVertexColor = factorVertexColor, 
                    factorEdgeColor = factorEdgeColor, factorClasses = factorClasses)
                  FactorVertices <- result$FactorVertices
                  FactorEdges <- result$FactorEdges
                  if ((is.null(edge.list))) {
                    from <- result$PairEdges[, 1]
                    to <- result$PairEdges[, 2]
                    edge.list <- two.to.pairs(from, to)
                  }
                }
                edgeList <- returnEdgeList(edge.list, Vertices, 
                  color = edgeColor, oriented = oriented)
                BlockList <- Args$blockList
                BlockTree <- Args$blockTree
                BlockEdges <- NULL
                if ((!is.null(BlockList) || !is.null(BlockTree))) {
                  message("Not tested!")
                  if (!(is.null(factors))) 
                    message("Edges between blocks and factors not implemented!")
                  if (is.null(BlockList) && !is.null(BlockTree)) 
                    BlockList <- blockTreeToList(BlockTree)
                  BlockEdges <- returnBlockEdgeList(edge.list, 
                    Vertices, BlockList, color = blockEdgeColor, 
                    oriented = oriented)
                }
                if (slave) 
                  Args$redrawGraphWindow(graphWindow = NULL, 
                    edgeList = edgeList, object = Object, factorVertexList = FactorVertices, 
                    factorEdgeList = FactorEdges, blockEdgeList = BlockEdges, 
                    title = title, ...)
                else Args$redrawGraphWindow(graphWindow = Args$graphWindow, 
                  edgeList = edgeList, object = Object, factorVertexList = FactorVertices, 
                  factorEdgeList = FactorEdges, blockEdgeList = BlockEdges, 
                  title = "Not used!", width = NULL, height = NULL, 
                  Arguments = Args)
            }
            CoCoLabelAllEdges <- function(object, slave = FALSE, 
                ...) {
                args <- list(...)
                Args <- args$Arguments
                getNodeName <- function(index, type) if (type == 
                  "Vertex") 
                  name(Args$vertexList[[index]])
                else if (type == "Factor") 
                  name(Args$factorVertexList[[abs(index)]])
                else if (type == "Block") 
                  label(Args$blockList[[abs(index)]])
                else NULL
                visitEdges <- function(edges) {
                  for (i in seq(along = edges)) {
                    vertices <- nodeIndicesOfEdge(edges[[i]])
                    types <- nodeTypesOfEdge(edges[[i]])
                    name.f <- getNodeName(vertices[1], types[1])
                    name.t <- getNodeName(vertices[2], types[2])
                    R <- testEdge(object, action = "remove", 
                      name.1 = name.f, name.2 = name.t, from = vertices[1], 
                      to = vertices[2], from.type = types[1], 
                      to.type = types[2], edge.index = i, force = force, 
                      Arguments = Args)
                    if (!is.null(R)) {
                      if (TRUE || (hasMethod("label", class(R)))) 
                        label(edges[[i]]) <- label(R)
                      if (TRUE || (hasMethod("width", class(R)))) 
                        width(edges[[i]]) <- width(R)
                    }
                  }
                  return(edges)
                }
                edgeList <- visitEdges(Args$edgeList)
                factorEdgeList <- visitEdges(Args$factorEdgeList)
                blockEdgeList <- visitEdges(Args$blockEdgeList)
                if (slave) 
                  Args$redrawGraphWindow(graphWindow = NULL, 
                    edgeList = edgeList, factorEdgeList = factorEdgeList, 
                    blockEdgeList = blockEdgeList, title = "A slave window", 
                    ...)
                else Args$redrawGraphWindow(graphWindow = Args$graphWindow, 
                  edgeList = edgeList, factorEdgeList = factorEdgeList, 
                  blockEdgeList = blockEdgeList, title = "Not used!", 
                  width = NULL, height = NULL, Arguments = Args)
            }
            CoCoMenu <- list(MainUser = list(label = "Position of \"vertices\"", 
                command = function(object, ...) print(Positions(list(...)$Arguments$vertexList))), 
                MainUser = list(label = "Label all edges, in this window", 
                  command = function(object, ...) CoCoLabelAllEdges(object, 
                    slave = FALSE, ...)), MainUser = list(label = "Label all edges, in slave window", 
                  command = function(object, ...) CoCoLabelAllEdges(object, 
                    slave = TRUE, ...)), MainUser = list(label = "Draw model, in this window", 
                  command = function(object, ...) {
                    Args <- list(...)$Arguments
                    ReturnVal <- modalDialog("Model entry modalDialog", 
                      "Enter number or tag", "last", graphWindow = Args$graphWindow)
                    print(ReturnVal)
                    if (ReturnVal == "ID_CANCEL") return()
                    model <- suppressWarnings(as.numeric(ReturnVal))
                    if (is.na(model)) model <- ReturnVal
                    CoCoDrawModel(object = model, slave = FALSE, 
                      ...)
                  }), MainUser = list(label = "Draw model, in slave window", 
                  command = function(object, ...) {
                    Args <- list(...)$Arguments
                    ReturnVal <- modalDialog("Model entry modalDialog", 
                      "Enter number or tag", "last", graphWindow = Args$graphWindow)
                    print(ReturnVal)
                    if (ReturnVal == "ID_CANCEL") return()
                    model <- suppressWarnings(as.numeric(ReturnVal))
                    if (is.na(model)) model <- ReturnVal
                    CoCoDrawModel(object = model, slave = TRUE, 
                      ...)
                  }), Vertex = list(label = "Test of user popup menu for vertices", 
                  command = function(object, name, ...) {
                    print(name)
                    print(c(list(...)$index))
                  }), Edge = list(label = "Test of user popup menu for edges", 
                  command = function(object, name1, name2, ...) {
                    args <- list(...)
                    print(c(name1, name2))
                    print(c(args$edge.index, args$from, args$to))
                  }), ClosedBlock = list(label = "Test of user popup menu for blocks", 
                  command = function(object, name, ...) {
                    print(name)
                    print(c(list(...)$index))
                  }), )

            Edges <- returnEdges(model = object, fix = "all.edges")

            args <- list(...)
	    doAdd <- FALSE
	    if (any(names(args) == "dynamicGraph")) {
	       doAdd <- TRUE
	       linkDynamicGraph <- args$dynamicGraph
            }

            if (doAdd) {
                if (is.null(list(...)$UserMenus)) 
                  addGraph(linkDynamicGraph, 
                           from = Edges[, 1], to = Edges[, 2],  
                           Object = object, UserMenus = CoCoMenu, 
                           ...)
                else addGraph(linkDynamicGraph, 
                              from = Edges[, 1], to = Edges[, 2],  
                              Object = object, ...)
            } else {
                VariableDescription <- returnVariableDescription(object = object, 
                    levels = FALSE)
                types <- validVertexClasses()[, 1][3 - 2 * VariableDescription$types]
                if (isClass("NodeProto")) {
                    if (is.null(list(...)$UserMenus)) 
                      DynamicGraph(names = VariableDescription$names, 
                        types = types, from = Edges[, 1], to = Edges[, 
                          2], object = object, UserMenus = CoCoMenu, 
                        ...)
                    else DynamicGraph(names = VariableDescription$names, 
                      types = types, from = Edges[, 1], to = Edges[, 
                        2], object = object, ...)
                }
                else {
                    warning("Remove objects of class 'GraphLatticeProto' and restart R.")
                }
            }
        })
    setClass("CoCoTestClass", representation(deviance = "numeric", 
        df = "numeric", p = "numeric"))
    if (!isGeneric("label") && !isGeneric("label", where = 2)) {
        if (is.function("label")) 
            fun <- label
        else fun <- function(object) standardGeneric("label")
        setGeneric("label", fun)
    }
    setMethod("label", "CoCoTestClass", function(object) format(object@p, 
        digits = 4))
    if (!isGeneric("width") && !isGeneric("width", where = 2)) {
        if (is.function("width")) 
            fun <- width
        else fun <- function(object) standardGeneric("width")
        setGeneric("width", fun)
    }
    setMethod("width", "CoCoTestClass", function(object) round(2 + 
        5 * (1 - object@p)))
    if (!isGeneric("testEdge")) {
        if (is.function("testEdge")) 
            fun <- testEdge
        else fun <- function(object, action, name.1, name.2, 
            ...) standardGeneric("testEdge")
        setGeneric("testEdge", fun)
    }
    setMethod("testEdge", signature(object = "CoCoModelClass"), 
        function(object, action, name.1, name.2, ...) {
            from.type <- args$from.type
            to.type <- args$to.type
            f <- function(type) if (is.null(type)) 
                ""
            else paste("(", type, ")")
            if (!is.null(args$Arguments$ArgBlocks) || (!is.null(args$Arguments$oriented) && 
                args$Arguments$oriented)) {
                message <- paste("Test of the edge from", name.1, 
                  "to", name.2, " is not implemented for causal models!!!")
                message(message)
                warning(message)
            }
            objectModel <- CoCoObjects::.recover.model(object)
            if (FALSE) {
                new.model <- subModifyModel(objectModel, action = "drop.edges", 
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
            return(newCoCoTestObject(test))
        })
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
            result <- CoCoRaw::editModel(action = action, modification = modification, 
                model = object@.model.number, result.form = result.form, 
                omit.test = TRUE, edges = section.2.edges, make.model = make.model, 
                return.test = return.test, push.pop = push.pop, 
                dispose = dispose, object = object)
            return(result)
        })
    if (!isGeneric("modifyModel")) {
        if (is.function("modifyModel")) 
            fun <- modifyModel
        else fun <- function(object, action, name, name.1, name.2, 
            ...) standardGeneric("modifyModel")
        setGeneric("modifyModel", fun)
    }
    setMethod("modifyModel", signature(object = "CoCoModelClass"), 
        function(object, action, name, name.1, name.2, ...) {
            args <- list(...)
            FactorVertices <- NULL
            FactorEdges <- NULL
            if (!is.null(args$Arguments$ArgBlocks)) 
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
                if (!is.null(args$Arguments) && (args$index > 
                  0) && !is.null(args$Arguments$ArgFactorVertices) && 
                  !is.null(args$Arguments$ArgVertices)) {
                  x <- (args$Arguments$ArgFactorVertices)
                  factors <- lapply(x, function(i) i@vertex.indices)
                  types <- lapply(x, function(i) class(i))
                  factors <- lapply(factors, function(x) x[x != 
                    args$index])
                  if (!(is.null(factors))) {
                    result <- returnFactorVerticesAndEdges(args$Arguments$ArgVertices, 
                      factors, types)
                    FactorVertices <- result$FactorVertices
                    FactorEdges <- result$FactorEdges
                  }
                }
                new.object <- subModifyModel(object, action = "drop.factor", 
                  modification = name, ...)
            }
            else if (action == "addVertex") {
                new.object <- subModifyModel(object, action = "add.interactions", 
                  modification = name, ...)
            }
            result <- list(object = new.object, FactorVertices = FactorVertices, 
                FactorEdges = FactorEdges)
            return(result)
        })
}
".onAttach" <-
function (lib, pkg) 
{
    if (!any(search() == "package:CoCoCg"))
      require(CoCo)
    # require(CoCoObjects)
    # require(tcltk)
    require(dynamicGraph)
}
".onLoad" <-
function (lib, pkg) 
{
    if (!any(search() == "package:CoCoCg"))
      require(CoCo)
    # require(CoCoObjects)
    require(tcltk)
    require(dynamicGraph)
    .First.lib.CoCoDynamicGraph(lib, pkg)
}
# ".packageName" <-
# "CoCoGraph"
