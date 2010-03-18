\name{coco.load}
\alias{coco.load}

\title{ Load CoCo }

\description{ Internal CoCoCore function:
              Load the dynamic linked or shared library with CoCo,
	      and assign necessary variables in \code{\link{.GlobalEnv}}. }

\usage{
coco.load(sh.lib.name = NULL)
}

\arguments{
  \item{sh.lib.name}{ The name of the library. }
}

\details{ This function is not to be called by the user. }

\value{ See \code{\link{dyn.load}}.
	Some teen variables starting with dot, ".",
	are assigned in \code{\link{.GlobalEnv}}.
}

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{coco.init}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
