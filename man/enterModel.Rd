\name{enterModel}
\alias{enterModel}

\title{ Enter a model }

\description{ Read a model into the CoCo object. }

\usage{
enterModel(model = "*", order = NULL, set = "*", homogeneous = FALSE, 
           data = NULL, object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{model}{ A character string with a generating class of the model,
		or the 3 generating classes for respectively the 
		discrete, linear and quadratic part of a mixed model. 
		A mixed model can also be given as a single generating class 
		corresponding to the cliques of the 2-section graph 
		of the model.  }
  \item{order}{ Numeric, if \code{order} is set then the model
		with only and all \code{order} interactions among variables
		in the argument \code{set} is entered. }
  \item{set}{ A character string with a set of variables. }
  \item{homogeneous}{ Logical, if \code{homogeneous} is TRUE then
		      a  homogeneous model is read of the single
		      generating class for a mixed model.}
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{makeModel}},
          \code{\link{editModel}},
          \code{\link{makeCurrent}},
          \code{\link{propertyModel}},
          and \code{\link{showModel}}. }

\examples{
library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);

enterModel(".", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel(order = 1, set = "*", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel(order = 2, set = "*", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel(order = 4, set = "*", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel("*", object = CoCoObject);
showModel("last", object = CoCoObject);

#enterModel("[[BD][BF][G][AD][AEF]]", object = CoCoObject);
 enterModel("[[BC][CE][F][AC][ADE]]", object = CoCoObject);
showModel("last", object = CoCoObject);

#enterModel("BD,BF,G,AD,AEF.;", object = CoCoObject);
 enterModel("BC,CE,F,AC,ADE.;", object = CoCoObject);
showModel("last", object = CoCoObject);

endCoCo(object = CoCoObject);

example.requiring.CoCoCg <- function() {
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

enterModel(".", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel(order = 1, set = "*", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel(order = 2, set = "*", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel("*", object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel("*", homogeneous = TRUE, object = CoCoObject);
showModel("last", object = CoCoObject);

enterModel("[[ab]] / [[aby][abx]] / [[x^][abxy][y^]]", object = CoCoObject);
showModel("last", object = CoCoObject);

endCoCo(object = CoCoObject);
}

library(CoCo);
data(Byssinosis38);
CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);

enterModel("*", object = CoCoObject);
enterModel("[[:byssinosis:dust:race:sex:smoking:employment]]", object = CoCoObject);

enterModel(paste(c("[[:byssinosis:dust:employment]", 
                   "[:dust:race:sex:employment]", 
                   "[:sex:smoking:employment]]"), collapse = ""), 
		   object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
