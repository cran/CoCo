\name{numberVariates}
\alias{numberVariates}

\title{ The number of variables in the CoCo object }

\description{ Return the number of variables entered into the CoCo object. }

\usage{
numberVariates(full = FALSE, data = NULL, 
               object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{full}{ Logical, if \code{full} is TRUE then the number
	       of specified variables is returned, that is
	       number without \code{\link{setUseVariables}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ Numeric }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{setUseVariables}} and \code{\link{showOptions}}. }

\examples{ 
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
numberVariates(object = CoCoObject);

Specification <- returnVariableDescription(object = CoCoObject);
setUseVariables(paste(Specification[[1]][c(1,3,4,6)], collapse = ""),
                object = CoCoObject);
enterTable(c(Reinis), object = CoCoObject);

numberVariates(object = CoCoObject);
numberVariates(full = TRUE, object = CoCoObject);

showOptions("specification", object = CoCoObject);

endCoCo(object = CoCoObject);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - #

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

numberVariates(object = CoCoObject);
numberVariates(full = TRUE, object = CoCoObject);

endCoCo(object = CoCoObject);

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
