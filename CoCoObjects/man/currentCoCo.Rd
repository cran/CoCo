\name{currentCoCo}
\alias{currentCoCo}
\alias{makeCurrentCoCo}

\title{ Make CoCo object to default CoCo object }

\description{ Mark a CoCo object as \code{.current.coco},
	      default CoCo object for the CoCo functions. }

\usage{
currentCoCo(object = CoCoCore::.currentCoCo())

makeCurrentCoCo(object.number)
}

\arguments{
  \item{object}{ See \code{\link[CoCoRaw]{endCoCo}}. }
  \item{object.number}{ Numeric, the number of the CoCo object. }
}

 \details{ 
The \code{a}-te row of \code{.coco.identifications}
is assigned to \code{.current.coco} in \code{.GlobalEnv}
by  \code{\link{makeCurrentCoCo}}.

A CoCo, a CoCoCg, and a model CoCo model object
is accepted as the argument \code{object} to \code{\link{currentCoCo}}.
}

\value{ TRUE }

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{  }

% \section{ToDo}{ }

\seealso{ \code{\link{currentCoCo}}. }

\examples{ 
library(CoCo);
data(Reinis);
CoCoReinisObject <- makeCoCo();
enterTable(Reinis, object = CoCoReinisObject);
fullReinisModel <- makeModel(enterModel("*", object = CoCoReinisObject));

library(CoCoCg);
data(Rats);
CoCoRatsObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoRatsObject);
fullRatsModel <- makeModel(enterModel("*", object = CoCoRatsObject));

showModel();
showModel(fullReinisModel);
makeCurrentCoCo(1);
showModel();

endCoCo(object = CoCoReinisObject);

makeCurrentCoCo(2);
endCoCo(object = CoCoRatsObject);
}

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
