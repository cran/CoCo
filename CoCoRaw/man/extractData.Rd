\name{extractData}
\alias{extractData}

\title{ Extract the data from a CoCo Object }

\description{ Extract the data from a CoCo Object. }

\usage{
extractData(full = FALSE, data = NULL, 
            object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{full}{ Logical, if \code{full} is TRUE then all
	       the varaibles are extracted, that is
	       as without \code{\link{setUseVariables}}. }
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

\seealso{ \code{\link{exportCoCo}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
