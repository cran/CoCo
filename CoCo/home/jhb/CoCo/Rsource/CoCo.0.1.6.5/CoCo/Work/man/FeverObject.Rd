\name{FeverObject}
\alias{FeverObject}
\docType{data}
\title{ ~~ data name/kind ... ~~}
\description{
  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(FeverObject)}
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
  ..$ names  : chr "RNHIL"
  ..$ levels : int [1:5] 2 2 2 2 4
  ..$ missing: int [1:5] 0 0 0 0 0
 - attr(*, ".medio")=List of 1
  ..$ type: NULL
 - attr(*, ".observations")=List of 3
  ..$ type  : chr "table"
  ..$ counts: num [1:2, 1:2, 1:2, 1:2, 1:4] 3 3 3 1 0 9 1 3 4 11 ...
  .. ..- attr(*, "dimnames")=List of 5
  .. .. ..$ R: chr [1:2] "1" "2"
  .. .. ..$ N: chr [1:2] "1" "2"
  .. .. ..$ H: chr [1:2] "1" "2"
  .. .. ..$ I: chr [1:2] "1" "2"
  .. .. ..$ L: chr [1:4] "1" "2" "3" "4"
  ..$ silent: logi TRUE
 - attr(*, ".structure")=List of 1
  ..$ type: NULL
 - attr(*, ".reference")= num 2.15e+09
 - attr(*, ".key")= chr "Spagnuolo,M., Pasternack, B. and Taranta, A  (1971) Risk of rheumatic fever recurrence after ... New Eng. J. Med. 285, 641-647. | __truncated__"
 - attr(*, ".type")= num 1
 - attr(*, ".title")= chr "Spagnuolo,M., Pasternack, B. and Taranta, A  (1971) Risk of rheumatic fever recurrence after ... New Eng. J. Med. 285, 641-647."
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
data(FeverObject)
## maybe str(FeverObject) ; plot(FeverObject) ...
}
\keyword{datasets}
