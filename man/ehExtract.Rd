\name{ehExtract}
\alias{ehExtract}

\title{ Extract models from the EH-procedure }

\description{ Copy in the CoCo object models from the classes of
	      the EH procedure to the model list. }

\usage{
ehExtract(class, sub.class = FALSE, data = NULL, 
          object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{class}{ A character string. 
		The argument \code{class} should be
	       % \code{"all"}: All four classes,
	       % \code{"duals"}: The two duals,
	       \code{"a.dual"} (the a-dual),
	       \code{"r.dual"} (the r-dual),
	       % \code{"classes"}: Accepted and rejected models,
	       \code{"accepted"} (accepted models), or
	       \code{"rejected"} (rejected models).
	  }
  \item{sub.class}{ A character string. See \code{\link{eh}}. }
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

\seealso{ \code{\link{makeCurrent}},
	  \code{\link{ehForceAccept}} and \code{\link{ehForceReject}}. }

\examples{
library(CoCo);
data(Byssinosis38);

CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);
eh(object = CoCoObject);
showModel("all", object = CoCoObject);
disposeOfModel("all", object = CoCoObject);
ehExtract(class = "accepted", object = CoCoObject);
showModel("all", object = CoCoObject);
disposeOfModel("all", object = CoCoObject);
ehExtract(class = "rejected", object = CoCoObject);
showModel("all", object = CoCoObject);
disposeOfModel("all", object = CoCoObject);
ehExtract(class = "a.dual", object = CoCoObject);
showModel("all", object = CoCoObject);
disposeOfModel("all", object = CoCoObject);
ehExtract(class = "r.dual", object = CoCoObject);
showModel("all", object = CoCoObject);
disposeOfModel("all", object = CoCoObject);
ehExtract(class = "a.dual", sub.class = "hierarchical", object = CoCoObject);
showModel("all", object = CoCoObject);
disposeOfModel("all", object = CoCoObject);
ehExtract(class = "r.dual", sub.class = "hierarchical", object = CoCoObject);
showModel("all", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
