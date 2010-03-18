
###  Copyright 2002 by Jens Henrik Badsberg.
###  License is granted to copy this program
###  under the GNU library public licence. 
###  This program comes with NO WARANTEE. 
###  See also the file COPYRIGHT for details.


### This defines the interface function to CoCo from R and Splus.

"makeCoCo"<- function(...) {
    make.coco(...)
  }

.First.lib <- function(lib, pkg) {
    require(methods)
    require(CoCoCore)
    require(CoCoObjects)
    require(CoCoRaw)
  }

".onAttach" <- function (lib, pkg) {
     require(tcltk)
     require(dynamicGraph)
    .First.lib.CoCoCore(lib, pkg)
    .First.lib.CoCoObjects(lib, pkg)
    .First.lib.CoCoDynamicGraph(lib, pkg)
  }

".onLoad" <- function (lib, pkg) {
 }

### 
### 56789012345678901234567890123456789012345678901234567890123456789012345678
