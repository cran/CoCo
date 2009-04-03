".packageName" <- "CoCo"

".First.lib" <-
function (lib, pkg) 
{
    require(methods)
    require(CoCoObjects)
    require(CoCoCore)
    require(CoCoRaw)
}

".onAttach" <-
function (lib, pkg) 
{
    require(CoCoRaw)

    # .First.lib.CoCoCore(lib, pkg)
    # .First.lib.CoCoObjects(lib, pkg)
    # .First.lib.CoCoDynamicGraph(lib, pkg)

    # message("'.onAttach' of CoCo")
}

".onLoad" <-
function (lib, pkg) 
{
    require(CoCoRaw)

    .First.lib.CoCoCore(lib, pkg)
    # .First.lib.CoCoObjects(lib, pkg)
    # .First.lib.CoCoDynamicGraph(lib, pkg)

    # message("'.onLoad' of CoCo")
}

"dG" <- function(object, ...) {
    require(CoCoGraph)
    dg(object, ...)
}
