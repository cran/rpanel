\name{flour_beetles}

\alias{flour_beetles}

\title{Toxins for the eradication of flour beetles}

\description{
The 'confused flour beetle' \emph{Tribolium confusum} can infest stored flour and grain. These data on the numbers of beetles killed by exposure to different concentrations of multiple toxins. The data were reported in an early paper by Strand (1930) on how to measure the toxicity of compounds. The trials of some compounds were replicated. Toxin concentration is measured in mg/l.
}

\references{
Strand, A.L. (1930). Measuring the toxicity of insect fumigants. Industrial & Engineering Chemistry Analytical Edition, 2, 4-8.
}

\examples{
\dontrun{
hacid <- subset(flour_beetles, Toxin == "Hydrocyanic_acid")
plot(Dead / (Living + Dead) ~ Concentration, data = hacid)
}}

\keyword{iplot}
\keyword{dynamic}

