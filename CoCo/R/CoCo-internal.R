".First.lib" <-
function (lib, pkg) 
{
    require(methods)
    require(CoCoCore)
    require(CoCoObjects)
    require(CoCoRaw)
}
".onAttach" <-
function (lib, pkg) 
{
    # require(tcltk)
    # require(dynamicGraph)
    # .First.lib.CoCoCore(lib, pkg)
    # .First.lib.CoCoObjects(lib, pkg)
    # .First.lib.CoCoDynamicGraph(lib, pkg)
    require(CoCoRaw)
}
".onLoad" <-
function (lib, pkg) 
{
    require(CoCoRaw)
    "dynamic.Graph" <- function(object, ...) {
        require(CoCoGraph)
        dynamic.Graph(object, ...)
    }
    .First.lib.CoCoCore(lib, pkg)
    # .First.lib.CoCoObjects(lib, pkg)
    # .First.lib.CoCoDynamicGraph(lib, pkg)
}
"dynamic.Graph" <- function(object, ...) {
    require(CoCoGraph)
    CoCoGraph::dynamic.Graph(object, ...)
}
