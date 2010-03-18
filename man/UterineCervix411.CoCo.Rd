\name{UterineCervix411}
\alias{UterineCervix411}
\docType{data}

\title{ Carcinoma of the uterine cervix }

\description{ Carcinoma of the uterine cervix. }

\usage{data(UterineCervix411)}

\format{
  The format is:
 int [1:77, 1:9] 10 1 2 1 1 8 1 1 1 2 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:9] "count" "a" "b" "c" ...
}

% \details{ }

\source{ A Handbook of Small Data Sets, edited by D.J. Hand et al., 
         from Chapman and Hall, ISBN 0 412 39920 2. }

% \references{ }

\examples{
library(CoCo);
data(UterineCervix411); UterineCervix411;
levels <- apply(UterineCervix411, 2, max)[-1]; missing <- c(rep(0, 7), 1);
CoCoObject <- makeCoCo();
enterNames(names(levels), levels - missing, missing = missing, 
			  object = CoCoObject);
enterList(c(t(UterineCervix411)), accumulated = TRUE, object = CoCoObject);
showOptions("specification", object = CoCoObject);
returnTable("sparse.table", object = CoCoObject);

showTable("observed", "*", output.form = "sparse.table", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
