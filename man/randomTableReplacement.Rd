\name{randomTableReplacement}
\alias{randomTableReplacement}

\title{ Replace the table of counts
	 with a random table with margins unchanged }

\description{ Replace the table observed counts with random table of counts
	      but with counts as the counts of the sufficient tables for the
	      decomposable model. }

\usage{
randomTableReplacement(model = "current", data = NULL, 
                       object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
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

\seealso{ \code{\link{enterDataFrame}}. }

\examples{ 
library(CoCo);
data(HairEyeColor);
CoCoObject <- makeCoCo();
enterTable(HairEyeColor, object = CoCoObject);
enterModel(":Hair:Sex,:Eye:Sex", object = CoCoObject);
returnTable("observed", "*", object = CoCoObject);
returnTable("observed", ":Hair:Sex", object = CoCoObject);
returnTable("observed", ":Eye:Sex", object = CoCoObject);

randomTableReplacement(object = CoCoObject);
returnTable("observed", "*", object = CoCoObject);
returnTable("observed", ":Hair:Sex", object = CoCoObject);
returnTable("observed", ":Eye:Sex", object = CoCoObject);

randomTableReplacement(object = CoCoObject);
returnTable("observed", "*", object = CoCoObject);
returnTable("observed", ":Hair:Sex", object = CoCoObject);
returnTable("observed", ":Eye:Sex", object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
