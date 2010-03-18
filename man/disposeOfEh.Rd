\name{disposeOfEh}
\alias{disposeOfEh}

\title{ Dispose of the model classes of the EH procedure }

\description{ Dispose of model classes of the EH procedure.
}

\usage{
disposeOfEh(code = "all", data = NULL, 
            object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{code}{ A character string. The argument
	       \code{code} should be
	       \code{"all"} (all four classes),
	       \code{"duals"} (the two duals),
	       \code{"a.dual"} (the a-dual),
	       \code{"r.dual"} (the r-dual),
	       \code{"classes"} (accepted and rejected models),
	       \code{"accepted"} (accepted models), or
	       \code{"rejected"} (rejected models).
	  }
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

\seealso{ \code{\link{eh}}. }

\examples{
library(CoCo);
data(Reinis);

CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
eh(strategy= "smallest", object = CoCoObject);
disposeOfEh(object = CoCoObject);
eh(strategy= "alternating", object = CoCoObject);
disposeOfEh(object = CoCoObject);
eh(strategy= "rough", object = CoCoObject);
disposeOfEh(object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
