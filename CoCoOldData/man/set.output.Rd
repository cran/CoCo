\name{set.output}
\alias{set.output}

\title{ Set CoCo output file }

\description{ Write standard output from CoCo on file. }

\usage{
set.output(file.name, object = CoCoCore::.currentCoCo())
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

\note{ This command is fragile:
       Redirects the standard output,
       and causes R to crash under Windows. }

% \section{ToDo}{....}

% \seealso{ \code{\link[CoCoRaw]{enterNames}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
