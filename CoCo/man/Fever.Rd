\name{Fever}
\alias{Fever}
\docType{data}

\title{ Risk of rheumatic fever recurrence after streptococcal infections }

\description{
         Risk of rheumatic fever recurrence after streptococcal
         infections, prospective study of clinical and social factors.
}

\usage{data(Fever)}

\format{
  The format is:
 num [1:2, 1:2, 1:2, 1:2, 1:4] 3 3 3 1 0 9 1 3 4 11 ...
 - attr(*, "dimnames")=List of 5
  ..\$ R: chr [1:2] "1" "2"
  ..\$ N: chr [1:2] "1" "2"
  ..\$ H: chr [1:2] "1" "2"
  ..\$ I: chr [1:2] "1" "2"
  ..\$ L: chr [1:4] "1" "2" "3" "4"
}

\details{
  \itemize{
    \item{1, Laboratory results}{:
       1 = ASO rise of 0 or 1 in presence of antibody rise, \\
       2 = ASO rise of 2 or 3 in presence of antibody rise, \\
       3 = ASO rise of 4,5  or 6 in presence of antibody rise, \\
       4 = ASO rise of 0 or 1 in absence of antibody rise, }

    \item{2, Interval from last attack}{:
       1 = less than 24 mounths, \\
       2 = 24 mounths or more, }

    \item{3, Heart disease}{:
       1 = Yes, \\
       2 = No, }

    \item{4, Number of previous attacks}{:
       1 = only the initial rheumatic fever attack, \\
       2 = more than the initial rheumatic fever attack, }

    \item{5, Recurrence of rheumatic fever}{:
       1 = Yes, \\
       2 = No, }
  }
}

\source{ From Example 3.8-1: Recurrence of rheumatic fever:
         Adjusted degrees of freedom for empty cells.
         Bishop, Fienberg and Holland (1975), page 117. }

\references{ Spagnuolo, M., Pasternack, B. and Taranta, A  (1971)
             Risk of rheumatic fever recurrence after streptococcal infections,
             prospective study of clinical and social factors.
             New Eng. J. Med. 285, 641-647. }

\examples{
data(Fever);
ftable(Fever, col.vars=3:5);

CoCoObject <- makeCoCo();
enterTable(Fever, object = CoCoObject);
eh(object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
