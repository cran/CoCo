\name{Byssinosis38}
\alias{Byssinosis38}
\docType{data}

\title{ Byssinosis }

\description{
  Byssinosis
}

\usage{data(Byssinosis38)}

\format{
  The format is:
 num [1:2, 1:3, 1:2, 1:2, 1:2, 1:3] 3 37 0 74 2 258 25 139 0 88 ...
 - attr(*, "dimnames")=List of 6
  ..\$ :byssinosis: chr [1:2] "1" "2"
  ..\$ :dust      : chr [1:3] "1" "2" "3"
  ..\$ :race      : chr [1:2] "1" "2"
  ..\$ :sex       : chr [1:2] "1" "2"
  ..\$ :smoking   : chr [1:2] "1" "2"
  ..\$ :employment: chr [1:3] "1" "2" "3"
}

% \details{ }

\source{ A Handbook of Small Data Sets, edited by D.J. Hand et al., 
         from Chapman and Hall, ISBN 0 412 39920 2. }

\references{ Higgens, J.E. and Koch, G.G. (1977), Variable selection
             and generalized chi-square analysis of categorical data
             applied to a large cross-sectional occupational health survey,
             International Statistical Reviews, 45, 51-62. }

\examples{
library(CoCo);
data(Byssinosis38);
ftable(Byssinosis38, col.vars=4:6);

CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);
eh(object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
