\name{cholesterol}
\docType{data}
\alias{cholesterol}

\title{Cholesterol and age for samples of women in Iowa and Nebraska}

\description{
Snedecor & Cochran (1967) report a subsample of measurements from an early study of health and nutrition by Swanson et al. (1955), with a particular focus on cholesterol. Women were recruited from both Iowa and Nebraska and there is interest in whether there is any evidence of a systematic difference between the groups. If there is no evidence of a difference, the groups might reasonably be pooled. An upward trend in cholesterol with age is expected.

The \code{cholesterol} dataframe consists of three columns of data:
\itemize{
\item \code{Age}: age in years;
\item \code{Cholesterol}: cholesterol in mg / 100 ml;
\item \code{Location}: The state of residence, Iowa or Nebraska.
}

}

\references{
   Snedecor, George W. and Cochran, William G. (1967). Statistical Methods. The Iowa State University Press, USA.
    Swanson, P., Levertost, R., Gram, M. R., Roberts, H. and Pesek, I. (1955). Blood values of women : cholesterol. Journal of Gerontology 10, 41-47.
   }

\examples{
\dontrun{
library(ggplot2)
ggplot(cholesterol, aes(Age, Cholesterol, col = Location)) + geom_point()
}}

\keyword{iplot}
\keyword{dynamic}

