\name{emOn}
\alias{emOn}

\title{ Handling of latent variables and missing values by the EM algorithm }

\description{ Turn on or off the use of the EM algorithm
	      for estimating values for missing values. }

\usage{
emOn(hit = FALSE, setslot = TRUE, data = NULL, 
     object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{hit}{ A character string. \code{hit} should be
	       \code{"on"}, \code{"off"}, or \code{"what"}. }
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

\seealso{ \code{\link{excludeMissing}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
