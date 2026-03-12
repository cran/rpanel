\name{Hyde}
\docType{data}
\alias{Hyde}

\title{Data on longevity and father's age, from the Hyde genealogy}

\description{
In addition to his pioneering work on the telephone, Alexander Graham Bell was also interested in heredity. He was one of the first people to investigate the relationship between longevity and the age of parents. His book 'The duration of life and conditions associated with longevity: a study of the Hyde genealogy' describes data from the descendants William Hyde, one of the early settlers of Norwich, Conn., who died in 1681. The data were retrieved from the 'Genealogy of the Hyde Family' by Reuben H. Waiworth, LL. D. (1864). The Appendix of the book by Bell refers to tables of detailed data published in Bell's own journal, the Beinn Bhreagh Recorder. These include Table 7: Age at death by father's age when person was born (B. B. Rec., IX, 143-171).

The \code{hyde} dataframe consists of three columns of data:
\itemize{
\item \code{Father}: age of the father at the birth of the child;
\item \code{Child}: age of the child at death;
\item \code{Frequency}: the number of cases with this combination of ages in the Hyde genealogy.
}

Access to these data was kindly facilitated by the Manuscript Division of the US Library of Congress where the Alexander Graham Bell family papers are held.
}

\references{
   Bell, A.G. (1918). The duration of life and conditions associated with longevity: a study of the Hyde genealogy. Genealogical Record Office.
   
   Alexander Graham Bell family papers, 1834-1974. US Library of Congress. https://lccn.loc.gov/mm76051268.
   
   Waiworth, R. H. (1864). Genealogy of the Hyde Family.
   }

\examples{
\dontrun{
ind  <- rep(1:nrow(Hyde), Hyde$Frequency)
Hyde <- Hyde[ind, 1:2]
hist(Hyde$Child)
}}

\keyword{iplot}
\keyword{dynamic}

