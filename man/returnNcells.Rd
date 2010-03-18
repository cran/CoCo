\name{returnNcells}
\alias{returnNcells}

\title{ Return the number of cells in a table }

\description{
	Return the number of cells in the table formed by the cross
	classification according to a given set of discrete variables.
}

\usage{
returnNcells(set = "*", full = FALSE, total = FALSE, data = NULL, 
             object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{set}{ A character string with a set of discrete variables. }
  \item{full}{ Logical, if \code{full} is TRUE then
	       all specified variables (\code{\link{setUseVariables}})
	       are considered. }
  \item{total}{ Logical, if \code{total} is TRUE then
		 also missing levels (\code{\link{excludeMissing}})
		 are counted. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\value{ An integer. }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{enterNames}},
	  \code{\link{setUseVariables}} and \code{\link{excludeMissing}}. }

\examples{

library(CoCo)
data(Hochberg77)
ftable(Hochberg77, col.vars=4:6)

# Without "data" require ending the CoCo object:

# returnNcells(data = Hochberg77);
# endCoCo();

# With CoCo object:

CoCoObject <- makeCoCo();

enterTable(Hochberg77, missing = c(1, 1, 0, 0, 0, 0), object = CoCoObject);

showOptions("specification", object = CoCoObject)

returnNcells(object = CoCoObject);

# With only a subset:

missing <- c(1, 1, 0, 0, 0, 0);
enterNames(names(dimnames(Hochberg77)), 
           levels = dim(Hochberg77) - missing,
           missing = missing, CoCoObject);

setUseVariables("bcdef.", object = CoCoObject)

enterTable(c(Hochberg77), object = CoCoObject);

showOptions("specification", object = CoCoObject)

returnNcells(object = CoCoObject);

returnNcells(set = "abc", object = CoCoObject);

returnNcells(set = "bc.", full = FALSE, total = FALSE, object = CoCoObject);
returnNcells(set = "bc.", full = FALSE, total = TRUE,  object = CoCoObject);
returnNcells(set = "abc.", full = TRUE,  total = FALSE, object = CoCoObject);
returnNcells(set = "abc.", full = TRUE,  total = TRUE,  object = CoCoObject);

excludeMissing("in", set = "bc.", object = CoCoObject);

returnNcells(set = "bc.", full = FALSE, total = FALSE, object = CoCoObject);
returnNcells(set = "bc.", full = FALSE, total = TRUE,  object = CoCoObject);
returnNcells(set = "abc.", full = TRUE,  total = FALSE, object = CoCoObject);
returnNcells(set = "abc.", full = TRUE,  total = TRUE,  object = CoCoObject);

endCoCo(object = CoCoObject);

# With continuous:

example.requiring.CoCoCg <- function() {
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
returnNcells(set = "ab;", object = CoCoObject);
returnNcells(object = CoCoObject); 

endCoCo(object = CoCoObject);
}
}
\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
