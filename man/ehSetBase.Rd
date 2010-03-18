\name{ehSetBase}
\alias{ehSetBase}

\title{ The base model of the EH procedure }

\description{ Set the base model of the EH procedure.
	      Tests of models in the EH procedure are performed
	      against the saturated model, or the
	      model entered by \code{ehSetBase}.
	      }

\usage{
ehSetBase(model = FALSE, data = NULL,
          object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ The base model of the EH procedure effects fixing in
       the EH procedure, \code{\link{ehForceFix}}.
       Terms not in the base model of the EH procedure
       are set to be fixed out for the EH procedure,
       e.i., the fixing out is set to the dual representation
       of the model set by \code{ehSetBase}.
       Since there is no distinction between fixing out due to
       \code{ehForceFix(gc, fix = "out", ...)} and due to
       \code{ehSetBase}, and since a larger EH base model
       would give less fixing out in the EH procedure,
       previous fixing out in the EH procedure is canceled
       by \code{ehSetBase}. 
       Use \code{ehForceFix(fix = "out", redo = TRUE, ...)} to
       redo the fixing out in the EH procedure.

       Only terms in the base model of the EH procedure
       can be fixed into the EH procedure. 
       Thus terms set by \code{ehForceFix(gc, fix = "in", ...)}
       might be reduced by \code{ehSetBase}.
       }

% \section{ToDo}{....}

\seealso{ \code{\link{eh}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
