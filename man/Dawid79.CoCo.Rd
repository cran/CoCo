\name{Dawid79}
\alias{Dawid79}

\docType{data}

\title{ Dawid and Skene (1979) }

\description{
  Data used in "Maximum likelihood estimation of 
                observed errorrates using the EM algorithm".
}

\usage{data(Dawid79)}

\format{
  The format is:
 int [1:45, 1:11] 1 1 1 1 1 1 1 1 1 1 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:11] "count" "v" "w" "a" ...
}

% \details{ }

\source{ Dawid, A. P. and Skene, A. M. (1979). Maximum likelihood
         estimation of observed errorrates using the EM algorithm.
         Appl. Statist. 28, 20-28. }

% \references{ }

\examples{
library(CoCo);
data(Dawid79); Dawid79;
levels <- apply(Dawid79, 2, max)[-1]; missing <- c(rep(0, 9), 1);
CoCoObject <- makeCoCo();
enterNames(names(levels), levels - missing, missing = missing, 
			  object = CoCoObject);
enterList(c(t(Dawid79)), accumulated = TRUE, object = CoCoObject);
showOptions("specification", object = CoCoObject);
showTable("observed", "*", output.form = "sparse.table", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
