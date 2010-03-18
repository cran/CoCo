\name{excludeMissing}
\alias{excludeMissing}

\title{ Handling of missing values by excluding cases }

\description{ The function controls whether cases with missing values
	      are excluded or if for discrete variables 
	      the levels marked as missing just are additional levels.
	      }

\usage{
excludeMissing(hit = "flop", set = ";", setslot = TRUE, data = NULL, 
               object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{hit}{ A character string.
	      \code{hit} should be
	      \code{"what"},
	      \code{"on"},
	      \code{"off"}, 
	      \code{"flop"} (for switching between \code{"on"} and
	      		      \code{"off"}), 
	      or \code{"in"}.}
  \item{set}{ A character string with the set \code{set} of variables
	      when \code{hit} is set to \code{"in"}. }
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

\seealso{ \code{\link{emOn}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
