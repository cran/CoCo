\name{recover.coco.model}
\alias{recover.coco.model}

\title{ Recover a CoCo model object }

\description{ Recover a CoCo model object. }

\usage{
recover.coco.model(coco.model.object, key = .return.key(coco.model.object), 
		   model = .return.model.of.object(coco.model.object), 
		   level = 90000, pos = .GlobalEnv, 
		   new.id = NULL, new.no = NULL)
}

\arguments{
  \item{coco.model.object}{ Object of class 
			    \code{\link{CoCoModelClass-class}}. }
  \item{key}{ The key of the object. }
  \item{model}{ The model of the object. }
  \item{level}{ Numeric for depth of call. }
  \item{pos}{ \code{.GlobalEnv}. }
  \item{new.id}{ Numeric, the new ideintification of the CoCo object. }
  \item{new.no}{ Numeric, the new model number. }
}

% \details{ }

% \value{ }

% \references{Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

\seealso{ \code{\link[CoCo]{makeCoCo}}. }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
