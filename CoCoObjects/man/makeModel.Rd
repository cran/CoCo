\name{makeModel}
\alias{makeModel}

\title{ Make a CoCo model object }

\description{ Make a CoCo model object }

\usage{
makeModel(model = FALSE, title = "", push.pop = FALSE, data = NULL, 
          object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link[CoCoRaw]{returnModel}}. }
  \item{title}{ A charater string with the \code{title} of the object. }
  \item{push.pop}{ Logical, if \code{push.pop} is TRUE, the model argument
		   is numeric and the object argument is a CoCoCg object
		   then model pointers are restored by less calls of CoCoCg. 
		   See also \code{\link[CoCoRaw]{returnModelNumber}}
		   and \code{\link[CoCoRaw]{makeCurrent}}. }
  \item{data}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
  \item{object}{ See \code{\link[CoCoRaw]{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link[CoCoRaw]{propertyModel}}. }
}

% \details{ }

\value{
  Object of class \code{\link{CoCoModelClass-class}}.
}

\references{Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

\seealso{ \code{\link[CoCo]{makeCoCo}}. }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
