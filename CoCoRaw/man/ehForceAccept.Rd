\name{ehForceAccept}
\alias{ehForceAccept}
\alias{ehForceReject}

\title{ Force models into the classes of the EH procedure }

\description{ Models are forced into a model class of the EH-procedure
	      regardless of whether the models are accepted or rejected.
}

\usage{
ehForceAccept(model, a = FALSE, b = FALSE, data = NULL, 
              object = .object.of.model(model, data = data, ...), ...)

ehForceReject(model, a = FALSE, b = FALSE, data = NULL, 
              object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{showModel}}. }
  \item{a}{ See \code{\link{showModel}}. }
  \item{b}{ See \code{\link{showModel}}. }
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
	  and \code{\link{ehFit}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
