\name{ehForceAddDualToClass}
\alias{ehForceAddDualToClass}

\title{ Force a dual into a class of models of the EH procedure }

\description{ Force a dual of the EH procedure into a class
	      without fitting and classifying the models. }

\usage{
ehForceAddDualToClass(dual = "a.dual", class = "accepted", 
                      sub.class = FALSE, data = NULL, 
                      object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{dual}{ A character string. The argument \code{dual} should be
	       \code{"a.dual"} (add the a-dual),
	       or \code{"r.dual"} (add the r-dual).
	       }
  \item{class}{ A character string. The argument \code{class} should be
	       \code{"accepted"} (add the dual to class of accepted models),
	       or \code{"rejected"} (add the dual to class of rejected models).
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

\seealso{ \code{\link{eh}} and \code{\link{ehFindDual}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
