
###  Copyright 2002 by Jens Henrik Badsberg.
###  License is granted to copy this program
###  under the GNU library public licence. 
###  This program comes with NO WARANTEE. 
###  See also the file COPYRIGHT for details.


### This defines the interface function to CoCo from R and Splus.

"makeCoCo"<-
  function(...)
  {
    make.coco(...)
  }

.First.lib <- function(lib, pkg) {
  require(methods)
  require(CoCoCore)
  require(CoCoObjects)
  require(CoCoRaw)
  }

### 
### 56789012345678901234567890123456789012345678901234567890123456789012345678
