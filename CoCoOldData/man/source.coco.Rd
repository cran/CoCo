\name{source.coco}
\alias{source.coco}

\title{ Read CoCo commands from file }

\description{ Read coco commands. }

\usage{
source.coco(file.name, object = CoCoCore::.currentCoCo())
}

\arguments{
  \item{file.name}{ A character string with the \code{file.name} 
		    of the file to read from. }
  \item{object}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
}

% \details{ }

\value{ TRUE }

\references{ Badsberg, J.H.: A guide to CoCo, JSS, 2001
             ( http://www.jstatsoft.org/v06/i04/ )
             and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

\note{ This command is fragile:
       Redirects the standard input,
       and causes R to crash under Windows. }

% \section{ToDo}{....}

\seealso{ \code{\link[CoCoRaw]{enterNames}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
