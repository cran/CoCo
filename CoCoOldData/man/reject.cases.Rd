\name{reject.cases}
\alias{reject.cases}
\alias{select.cases}
\alias{or.reject.cases}
\alias{or.select.cases}

\title{ Declare a subset of the cases to be read }

\description{ Declare a subset of the cases to be read. }

\usage{
reject.cases(set, cell, object = .current.coco)

select.cases(set, cell, object = .current.coco)

or.reject.cases(set, cell, object = .current.coco)

or.select.cases(set, cell, object = .current.coco)
}

\arguments{
  \item{set}{ A character string with the names of the variables. }
  \item{cell}{ A numeric vector with the levels of the variables. }
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
