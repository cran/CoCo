\name{Scrotal94}
\alias{Scrotal94}
\docType{data}

\title{ Scrotal Swelling Data }

\description{ Scrotal Swelling Data }

\usage{data(Scrotal94)}

\format{
  The format is:
 num [1:2, 1:2, 1:2, 1:2, 1:2, 1:2, 1:2, 1:2] 0 0 16 0 0 0 38 0 0 0 ...
 - attr(*, "dimnames")=List of 8
  ..\$ H: chr [1:2] "1" "2"
  ..\$ A: chr [1:2] "1" "2"
  ..\$ B: chr [1:2] "1" "2"
  ..\$ C: chr [1:2] "1" "2"
  ..\$ D: chr [1:2] "1" "2"
  ..\$ E: chr [1:2] "1" "2"
  ..\$ F: chr [1:2] "1" "2"
  ..\$ G: chr [1:2] "1" "2"
}
\details{
  \itemize{
    \item{H}{: Hernia, }
    \item{A}{: Possible to get above the swelling, }
    \item{B}{: Swelling transilluminates, }
    \item{C}{: Swelling separated from testes, }
    \item{D}{: Positive Valsalva, }
    \item{E}{: Tenderness, }
    \item{F}{: Pain, }
    \item{G}{: Urinal tract infection. }
  }
}

\source{ David Madigan and Adrian E. Raftery (1994):
         Model Selection and Accounting for Model Uncertainty
         in Graphical Models Using Occam's Window.
         J. Am. Statist. Assoc. 428, 1535-1546. }

% \references{ }

\examples{
library(CoCo);
data(Scrotal94);
ftable(Scrotal94, col.vars=5:8);

CoCoObject <- makeCoCo();
enterTable(Scrotal94, object = CoCoObject);
eh(object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
