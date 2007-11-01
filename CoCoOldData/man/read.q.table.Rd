\name{read.q.table}
\alias{read.q.table}
\alias{read.q.list}

\title{ Read Q tables, structural zeros, from file }

\description{ Read Q table. }

\usage{
read.q.table(set, setslot = TRUE, object = CoCoCore::.currentCoCo())

read.q.list(set, setslot = TRUE, object = CoCoCore::.currentCoCo())
}

\arguments{
  \item{set}{ A character string with the names 
	      of the \code{set} of variables. }
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

\seealso{ \code{\link[CoCoRaw]{enterNames}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
