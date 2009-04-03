\name{clearCoCoObjects}
\alias{clearCoCoObjects}

\title{ Clear CoCo object }

\description{ Clear CoCo objects. 

When resuming a R-session with CoCo-objects,
please use 'library(CoCo)' and/or 'library(CoCoCg)' be using CoCo-objects.
(Currently 'require(CoCo)' and/or 'require(CoCoCg)' when loading
CoCoObject and/or CoCoGraph does not agree with 'R CMD check CoCo'.)
}

\usage{
clearCoCoObjects(coco.object = NULL, silent = FALSE, pos = .GlobalEnv, 
          printWarnings = FALSE) 

}

\arguments{
  \item{coco.object}{ See \code{\link[CoCoRaw]{endCoCo}}. }
  \item{pos}{ The envirinment of the CoCo object. }
  \item{silent}{ Logical. }
  \item{printWarnings}{ Logical. }
}

% \details{ }

% \value{ TRUE }

% \references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{  }

% \section{ToDo}{ }

% \seealso{ }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
