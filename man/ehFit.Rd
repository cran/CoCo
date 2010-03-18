\name{ehFit}
\alias{ehFit}

\title{ Fit and classify models in the EH procedure }

\description{ Fit and classify models in the EH procedure.
	      The models to fit can be
	      models from the current duals of the EH procedure
	      or models from the model list of the CoCo object. 
	      Only a single "step" of the EH-procedure is performed.
	      }

\usage{
ehFit(model = FALSE, a = FALSE, b = FALSE, dual = NULL, 
      sub.class = FALSE, p.accepted = FALSE, data = NULL, 
      object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{showModel}}. }
  \item{a}{ See \code{\link{showModel}}. }
  \item{b}{ See \code{\link{showModel}}. }
  \item{dual}{ A character string. The argument \code{dual} should be
	       \code{"both"} (fit both the two duals),
	       \code{"smallest.dual"} (fit the smallest dual),
	       \code{"largest.dual"} (fit the largest),
	       \code{"a.dual"} (fit the a-dual),
	       or \code{"r.dual"} (fit the r-dual).
	  }
  \item{sub.class}{ A character string. See \code{\link{eh}}. }
  \item{p.accepted}{ A numeric. See \code{\link{eh}}. }
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


\seealso{ \code{\link{eh}}, \code{\link{showModel}},
	  \code{\link{ehForceAccept}}, 
	  and \code{\link{ehForceReject}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
