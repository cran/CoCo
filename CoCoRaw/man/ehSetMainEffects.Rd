\name{ehSetMainEffects}
\alias{ehSetMainEffects}

\title{ Set the main effects of the models in the EH procedure }

\description{ Set which variables to admit in the models in the EH procedure. }

\usage{
ehSetMainEffects(set = "what", data = NULL, 
                 object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{set}{ A character string with the names of the variables. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE, or a character string if the effects are returned. }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ CoCo will work more efficiently,
       if \code{\link{setUseVariables}} is used to restrict
       the EH procedure to a subset of the variables,
       than if \code{\link{ehSetMainEffects}}, \code{\link{ehSetBase}}
       or \code{\link{ehForceFix}} is used. }

% \section{ToDo}{....}

\seealso{ \code{\link{eh}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
