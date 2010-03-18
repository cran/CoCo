\name{ehFindDual}
\alias{ehFindDual}

\title{ Find duals in the EH procedure }

\description{ Find the a-dual, the r-dual, or both duals in the EH procedure
	      without fitting the models. }

\usage{
ehFindDual(dual = "both", sub.class = FALSE, data = NULL, 
           object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{dual}{ A character string. The argument \code{dual} should be
	       \code{"both"} (find both the two duals),
	       \code{"a.dual"} (find the a-dual),
	       or \code{"r.dual"} (find the r-dual).
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

\seealso{ \code{\link{eh}} and \code{\link{ehForceAddDualToClass}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
