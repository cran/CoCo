\name{read.data}
\alias{read.data}
\alias{read.specification}
\alias{read.names}
\alias{read.factors}
\alias{read.observations}
\alias{read.list}
\alias{read.table.coco}

\title{ Read data from files or standard input }

\description{ Read data from files or standard input. }

\usage{
read.data(setslot = TRUE, object = .current.coco)

read.specification(setslot = TRUE, object = .current.coco)

read.names(setslot = TRUE, object = .current.coco)

read.factors(setslot = TRUE, object = .current.coco)

read.observations(setslot = TRUE, object = .current.coco)

read.list(silent = FALSE, setslot = TRUE, object = .current.coco)

read.table.coco(silent = FALSE, setslot = TRUE, object = .current.coco)
}

\arguments{
  \item{silent}{ Logical, if \code{silent} then CoCo is more silent. }
  \item{setslot}{ See \code{\link[CoCoRaw]{enterNames}}. }
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

\seealso{ \code{\link[CoCoRaw]{set.data.file}} and
	  \code{\link[CoCoRaw]{enterNames}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
