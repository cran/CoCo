\name{setUseVariables}
\alias{setUseVariables}

\title{ Declare which variables to enter }

\description{
	Between entering specification and observations,
	declare which variables to enter into the CoCo object.
}

\usage{
setUseVariables(hit = "all", set = ";", setslot = FALSE, data = NULL,
                object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{hit}{ Character string with \code{"all"}, \code{"what"}
	       or a set of variables. }
  \item{set}{ Character string with set of variables. }
  \item{setslot}{  See \code{\link{enterNames}}. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ A character sting with the variables \code{set} to use.  }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{returnNcells}} for example. }

\examples{
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
numberVariates(object = CoCoObject);

Specification <- returnVariableDescription(object = CoCoObject);
setUseVariables(paste(Specification$names[c(1,3,4,6)], collapse = ""), 
                object = CoCoObject);
enterTable(c(Reinis), object = CoCoObject);
numberVariates(object = CoCoObject);
numberVariates(full = TRUE, object = CoCoObject);

showOptions("specification", object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
