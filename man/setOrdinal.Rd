\name{setOrdinal}
\alias{setOrdinal}

\title{ Declare some of the discrete variables to be ordinal }

\description{ Declare some of the discrete variables to be ordinal. }

\usage{
setOrdinal(set = "*", setslot = TRUE, data = NULL, 
           object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{set}{ A character string with the ordinal variables \code{set}. }
  \item{setslot}{ Logical, if \code{setslot} is TRUE then
                  \code{set} is added to \code{object}. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ A character sting with the ordinal variables \code{set}.  }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\seealso{\code{\link{enterDataFrame}},
         \code{\link{partialAssociations}}, \code{\link{optionsCoCo}},
         \code{\link{showDeviance}},
         \code{\link{returnTest}}, \code{\link{returnDeviance}}, 
         \code{\link{backward}} and \code{\link{forward}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
