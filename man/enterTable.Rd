\name{enterTable}
\alias{enterTable}

\title{ Enter a table of counts of the tables formed by
	cross classifying discrete variables }

\description{ Enter a table of counts of the tables formed by
              cross classifying discrete variables. 
	      If the input is a vector of the counts
	      the table has to be declared by \code{\link{enterNames}}
	      before the use of this function.
	      An \code{\link{array}} of counts with \code{\link{dimnames}}
	      can be entered by \code{\link{enterTable}} without
	      first specifying the variables in the CoCo object.
	      }

\usage{
enterTable(counts, silent = TRUE, setslot = TRUE, missing = NULL, 
           data = NULL, object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{counts}{ Some integers, either a vector
		 or a object of class \code{"array"}.
		 If \code{counts} is a vector then the variables
		 should be defined in the CoCo \code{object} by
		 \code{\link{enterNames}}.
		 If \code{counts} is of class \code{"array"} then
		 the names and number of levels for the variables
		 are extracted from the argument \code{counts}.
		 }
  \item{silent}{ Logical, is \code{silent} CoCo will be more
		 quiet about the cases read. }
  \item{missing}{ A numeric vector of length equal to the
		  number of variables.
		  If \code{counts} is of class \code{"array"}
		  and \code{missing} is not \code{"NULL"} then
		  \code{missing} will define missing levels as
		  \code{\link{enterNames}}. }
  \item{setslot}{ See \code{\link{enterNames}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterNames}}. }

\examples{
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

endCoCo(object = CoCoObject);

library(CoCo);
data(Byssinosis38);
CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
