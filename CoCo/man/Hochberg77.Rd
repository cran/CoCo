\name{Hochberg77}
\alias{Hochberg77}
\docType{data}

\title{ Highway safety }

\description{ Highway safety. }

\usage{data(Hochberg77)}

\format{
  The format is:
 num [1:3, 1:3, 1:2, 1:2, 1:2, 1:2] 407  62   0  45   7 ...
 - attr(*, "dimnames")=List of 6
  ..\$ a: chr [1:3] "1" "2" "3"
  ..\$ b: chr [1:3] "1" "2" "3"
  ..\$ c: chr [1:2] "1" "2"
  ..\$ d: chr [1:2] "1" "2"
  ..\$ e: chr [1:2] "1" "2"
  ..\$ f: chr [1:2] "1" "2"
}

\details{
  \itemize{
    \item{a}{: belt use recorded by non-police devices, }
    \item{b}{: driver injury recorded by non-police devices, }
    \item{c}{: driver's sex, }
    \item{d}{: car damage, }
    \item{e}{: belt use recorded by police, }
    \item{f}{: driver injury recorded by police. }
  }
}

\source{ Zhi Geng and Chooichiro Asano (1988):
         Recursive procedures for hierarchical loglinear
         models on high-dimensional contingency tables.
         J. Japanese Soc. Comp. Statist, 17-26. }

\references{ Hochberg, Y (1977):
             On the use of double sampling schemes in analyzing
             categorical data with misclassification errors.
             J. Am. Statist. Assoc. 72, 914-921. }

\examples{
data(Hochberg77)
ftable(Hochberg77, col.vars=4:6);

CoCoObject <- makeCoCo();
enterTable(Hochberg77, missing = c(1, 1, rep(0, 4)), object = CoCoObject);
excludeMissing("on", object = CoCoObject);
eh(object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
