\name{Byssinosis38Object}
\alias{Byssinosis38Object}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Byssinosis38Object)}
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
  ..$ names  : chr ":byssinosis:dust:race:sex:smoking:employment"
  ..$ levels : int [1:6] 2 3 2 2 2 3
  ..$ missing: int [1:6] 0 0 0 0 0 0
 - attr(*, ".medio")=List of 1
  ..$ type: NULL
 - attr(*, ".observations")=List of 3
  ..$ type  : chr "table"
  ..$ counts: num [1:2, 1:3, 1:2, 1:2, 1:2, 1:3] 3 37 0 74 2 258 25 139 0 88 ...
  .. ..- attr(*, "dimnames")=List of 6
  .. .. ..$ :byssinosis: chr [1:2] "1" "2"
  .. .. ..$ :dust      : chr [1:3] "1" "2" "3"
  .. .. ..$ :race      : chr [1:2] "1" "2"
  .. .. ..$ :sex       : chr [1:2] "1" "2"
  .. .. ..$ :smoking   : chr [1:2] "1" "2"
  .. .. ..$ :employment: chr [1:3] "1" "2" "3"
  ..$ silent: logi TRUE
 - attr(*, ".structure")=List of 1
  ..$ type: NULL
 - attr(*, ".reference")= num 2.15e+09
 - attr(*, ".key")= chr "Byssinosis: Higgens, J.E. and Koch, G.G. (1977), Variable selection and generalized chi-square ..., Int. Stat. Revies, 45, 51-62| __truncated__"
 - attr(*, ".type")= num 1
 - attr(*, ".title")= chr "Byssinosis: Higgens, J.E. and Koch, G.G. (1977), Variable selection and generalized chi-square ..., Int. Stat. Revies, 45, 51-62"
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
data(Byssinosis38Object)
## maybe str(Byssinosis38Object) ; plot(Byssinosis38Object) ...
}
\keyword{datasets}
