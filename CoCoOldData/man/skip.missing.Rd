\name{skip.missing}
\alias{skip.missing}

\title{ Declare that cases with missing values are skipped during reading }

\description{ Set CoCo to skip cases with any of the entered variables
	      with missing values.
	      The function has to be used between entering the specification
	      and entering the observations.
}

\usage{
skip.missing(object = CoCoCore::.currentCoCo())
}

\arguments{
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

\seealso{ \code{\link[CoCoRaw]{read.data}},
	  \code{\link[CoCoRaw]{enterNames}},
	  \code{\link[CoCoRaw]{enterList}},
	  \code{\link[CoCoRaw]{enterTable}}, and
	  \code{\link[CoCoRaw]{excludeMissing}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
