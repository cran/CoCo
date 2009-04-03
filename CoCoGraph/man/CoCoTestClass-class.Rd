\name{CoCoTestClass-class}
\docType{class}
\alias{CoCoTestClass-class}
\alias{label,CoCoTestClass-method}
\alias{width,CoCoTestClass-method}
\alias{setSlots,CoCoTestClass-method}
\alias{initialize,CoCoTestClass-method}

\title{CoCoTestClass Class}

\description{The class for the objects with test results of CoCo.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoCoTestClass", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{deviance}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{df}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{p}:}{Object of class \code{"numeric"} ~~ }
  }
}

\section{Methods}{
  \describe{
    \item{label}{\code{signature(object = "CoCoTestClass")}:
				 The \code{label} of the test for edges. }
    \item{width}{\code{signature(object = "CoCoTestClass")}:
				 The \code{width} of the test for edges. }
    \item{setSlots}{\code{signature(object = "CoCoTestClass")}: ... }
    \item{initialize}{\code{signature(object = "CoCoTestClass")}: ... }
  }
}

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

\seealso{ \code{\link[CoCo]{makeCoCo}}. }

% \examples{ }

\keyword{classes}
