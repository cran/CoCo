\name{set.data.file}
\alias{set.data.file}
\alias{set.specification.file}
\alias{set.observations.file}

\title{ Declare files to read data from }

\description{ Assign data files. }

\usage{
set.data.file(file.name, object = .current.coco)

set.specification.file(file.name, object = .current.coco)

set.observations.file(file.name, object = .current.coco)
}

\arguments{
  \item{file.name}{ A character string with the \code{file.name}. }
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
