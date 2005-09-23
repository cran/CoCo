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
    # "dG" <- function(object, ...) {
    #     require(CoCoGraph)
    #     dg(object, ...)
    # }
    .First.lib.CoCoCore(lib, pkg)
    # .First.lib.CoCoObjects(lib, pkg)
    # .First.lib.CoCoDynamicGraph(lib, pkg)
}
"dG" <- function(object, ...) {
    require(CoCoGraph)
    # CoCoGraph::dg(object, ...)
    dg(object, ...)
}
