\name{returnVariableDescription}
\alias{returnVariableDescription}

\title{ Return the specification of the data in the CoCo object }

\description{ Return the specification of the data in the CoCo object
	      in form of variable names, number of levels,
	      number of levels marked as missing value and type of variable. }

\usage{
returnVariableDescription(full = FALSE, levels = TRUE, 
                          missing.levels = TRUE, data = NULL, 
                          object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{full}{ Logical, if \code{full} is TRUE then the number
	       of specified variables is returned, that is
	       number without \code{\link{setUseVariables}}. }
  \item{levels}{ Logical, if \code{levels} is TRUE
		 then return the number of levels. }
  \item{missing.levels}{ Logical, if \code{missing.levels}
			  is TRUE then return the number of levels
			  marked as missing. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{
  A list with the components
  \item{names         }{ chr  }
  \item{labels        }{ NULL }
  \item{types         }{ int  }
  \item{stratum       }{ NULL }
  \item{levels        }{ int  }
  \item{missing.levels}{ int  }
}

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{forward}}. }

\examples{ 
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

showOptions("specification", object = CoCoObject);
Specification <- returnVariableDescription(object = CoCoObject);
Specification

endCoCo(object = CoCoObject);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - #

example.requiring.CoCoCg <- function() {
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

numberVariates(object = CoCoObject);
numberVariates(full = TRUE, object = CoCoObject);

endCoCo(object = CoCoObject);
}

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - #

library(CoCo);
data(Byssinosis38);
CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);

numberVariates(object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
