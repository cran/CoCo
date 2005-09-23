\name{Fuchs82Object}
\alias{Fuchs82Object}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Fuchs82Object)}
\format{
  The format is:
 list()
 - attr(*, ".parameters")=List of 5
  ..$ size       : Named num [1:7]  2048  1024   128 65536 65536 ...
  .. ..- attr(*, "names")= chr [1:7] "n" "p" "q" "r" ...
  ..$ location   : num [1:2] 700 550
  ..$ manager    : logi TRUE
  ..$ silent     : logi FALSE
  ..$ sh.lib.name: NULL
 - attr(*, ".invalid")=List of 1
  ..$ type: NULL
 - attr(*, ".specification")=List of 4
  ..$ type   : chr "names"
  ..$ names  : chr "gasdpm"
  ..$ levels : int [1:6] 2 2 2 2 2 2
  ..$ missing: int [1:6] 0 0 0 0 1 1
 - attr(*, ".medio")=List of 1
  ..$ type: NULL
 - attr(*, ".observations")=List of 3
  ..$ type  : chr "table"
  ..$ counts: num [1:2, 1:2, 1:2, 1:2, 1:3, 1:3] 0 2 5 3 0 0 2 1 1 0 ...
  .. ..- attr(*, "dimnames")=List of 6
  .. .. ..$ g: chr [1:2] "1" "2"
  .. .. ..$ a: chr [1:2] "1" "2"
  .. .. ..$ s: chr [1:2] "1" "2"
  .. .. ..$ d: chr [1:2] "1" "2"
  .. .. ..$ p: chr [1:3] "1" "2" "3"
  .. .. ..$ m: chr [1:3] "1" "2" "3"
  ..$ silent: logi TRUE
 - attr(*, ".structure")=List of 1
  ..$ type: NULL
 - attr(*, ".reference")= num 2.15e+09
 - attr(*, ".key")= chr "Fuchs, C., Maximum likelihood estimation and model selection in contingency tables with missing data, JASS. 77(378), 1982  /  Th| __truncated__"
 - attr(*, ".type")= num 1
 - attr(*, ".title")= chr "Fuchs, C., Maximum likelihood estimation and model selection in contingency tables with missing data, JASS. 77(378), 1982"
 - attr(*, "class")= atomic [1:1] CoCoClass
  ..- attr(*, "package")= chr "CoCoObjects"
}
\details{
  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Fuchs82Object)
## maybe str(Fuchs82Object) ; plot(Fuchs82Object) ...
}
\keyword{datasets}
