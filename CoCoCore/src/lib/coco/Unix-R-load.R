
## Directory of CoCo ('COCOHOME'):
COCOHOME   <- "/usr/lib/R/library/CoCo"

## Directory of object-file (now 'COCOLIB'):
COCOLIB    <- "/usr/lib/R/library/CoCo/lib/coco"

## Directory of R- and S-files (now 'RSCOCOLIB'):
RSCOCOLIB  <- "/usr/lib/R/library/CoCo/R+S/R.and.S"

## Name of object-file, without extension:
RCSHLIB    <- "librcoco"

file.path <- function(..., fsep="/") paste(..., sep=fsep)

source(file.path(RSCOCOLIB, "Unix-R-files.R"))

