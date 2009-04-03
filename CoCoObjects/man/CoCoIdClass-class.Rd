\name{CoCoIdClass-class}
\docType{class}
\alias{CoCoIdClass-class}

\title{CoCoIdClass Class}

\description{
The functions for the CoCo objects are found mainly
in the package \link[CoCoRaw]{CoCoRaw-package}.
A few function for CoCo objects are found in
\link[CoCoObjects]{CoCoObjects-package} and 
\link[CoCoGraph]{CoCoGraph-package}. 

Datasets are found in the packages \link[CoCo]{CoCo-package} and
\link[CoCoCg]{CoCoCg-package}.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoCoIdClass", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.reference}:}{Object of class \code{"numeric"},
			      see \code{\link{CoCoClass-class}}. }
    \item{\code{.id.env}:}{Object of class \code{"character"},
			   see \code{\link{CoCoClass-class}}. }
    \item{\code{.key}:}{Object of class \code{"character"},
			see \code{\link{CoCoClass-class}}. }
    \item{\code{.type}:}{Object of class \code{"numeric"},
			 see \code{\link{CoCoClass-class}}. }
    \item{\code{.title}:}{Object of class \code{"character"},
			  see \code{\link{CoCoClass-class}}. }
  }
}

\section{Methods}{
No methods defined with class "CoCoIdClass" in the signature.
}

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{  }

\seealso{
  \code{\link{CoCoClass-class}}, 
  % \code{\link{CoCoIdClass-class}}, 
  \code{\link{CoCoModelClass-class}}, 
  \code{\link[CoCoCore]{coco.init}}, 
  or \code{\link[dynamicGraph]{GraphLatticeProto-class}}.
}

% \examples{ }

\keyword{classes}
