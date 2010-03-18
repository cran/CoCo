\name{ok.coco}
\alias{ok.coco}
\alias{ok.coco.start}

\title{ Test if the call to CoCo succeeded }

\description{
  Internal CoCoCore function: Reports the warning message
  and returns FALSE, if the call to CoCo is not succeeded,
  else return TRUE.
}

\usage{
ok.coco(result, no.warnings = NULL)
ok.coco.start(result)
}

\arguments{
  \item{result}{ Structure with result of call to CoCo. }
  \item{no.warnings}{ Boolean for suppresion of warnings. }
}

\details{ This function is not to be called by the user. }

\value{ Logical }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
