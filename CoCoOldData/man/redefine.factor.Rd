\name{redefine.factor}
\alias{redefine.factor}

\title{ Change the number of declared levels for a discrete variable }

\description{
  Redefine the number of levels for a variable.
}

\usage{
redefine.factor(name, levels, missing.levels = 0, object = CoCoCore::.currentCoCo())
}

\arguments{
  \item{name}{ A character string with the \code{name} of the variable. }
  \item{levels}{ A integer with the new number of \code{levels}. }
  \item{missing.levels}{ A integer with the new number of
			 \code{missing.levels}. }
  \item{object}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
}

% \details{ }

\value{ TRUE }

\references{ Badsberg, J.H.: A guide to CoCo, JSS, 2001
             ( http://www.jstatsoft.org/v06/i04/ )
             and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ ~~further notes~~ }

% \section{ToDo}{....}

\seealso{ \code{\link[CoCoRaw]{read.data}} and
	  \code{\link[CoCoRaw]{enterNames}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
