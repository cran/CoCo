\name{returnModelNumber}
\alias{returnModelNumber}

\title{ Return the number of the CURRENT, BASE, or the LAST model }

\description{
  Return the internal model number of the CURRENT, the BASE. or the LAST model
  in the CoCo object.
}

\usage{
returnModelNumber(model = FALSE, no.warnings = FALSE, pop = FALSE, data = NULL, 
		  object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ The text string \code{"base"},
                \code{"current"} or \code{"last"}. }
  \item{no.warnings}{ Logical, if \code{no.warnings} is set to TRUE then
	no warnings is given if the model is not available. }
  \item{pop}{ Logical: If \code{pop} is set to TRUE in CoCoCg
	then the CURRENT and BASE pointers are set to the top pointers
	of the stack of CURRENT and BASE pointers in the CoCo object,
        see \code{push} at \code{\link{makeCurrent}}. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

\value{ An integer with the model number, if the model is avaliable,
        else NULL. }

\references{Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ \code{pop} is only implemented for CoCoCg objects. }

\seealso{\code{\link{enterModel}}, \code{\link{showModel}},
         \code{\link{returnTest}},
         \code{\link{isSubmodel}}, \code{\link{propertyModel}}, 
         \code{\link{makeCurrent}} and \code{\link{returnModel}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
