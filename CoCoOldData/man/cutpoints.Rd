\name{cutpoints}
\alias{cutpoints}

\title{ Reduce number of possible values for discrete variable }

\description{ Define cutpoints to transform a continuous variable
	      into a discrete variable in CoCo. 
	      The discrete variable must prior to the use of this
	      function be declared with the number of levels as
	      the cutpoints will result in.
	      }

\usage{
cutpoints(name, cutpoints, object = .current.coco)
}

\arguments{
  \item{name}{ A character string with the \code{name} of the variable. }
  \item{cutpoints}{ A vector of reals with the \code{cutpoints}. }
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
