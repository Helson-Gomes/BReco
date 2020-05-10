\name{get_fires}
\alias{get_fires}
\title{A function to download data on annual number of fire focus in Brazil}
\usage{
  get_fires(level = NULL, panel = FALSE)
}
\description{
  Download data from Fire Information for Resource Management System (FIRMS) to Brazil. The data can be downloaded for the national, state and municipal level.
  The data is referred to the sum of fire focus in each level choosed.
}
\arguments{
  \item{level}{The geographic level of interest. This opition can be ("country", "state", and "municipality").}
  \item{panel}{Use panel = TRUE if you want to download the dataset in a panel data format and panel = FALSE otherwise. The default value is FALSE}
}
\value{
\item{sigla.state}{The state name abbreviation. This variable will not appear if you choose the option level = "country" or level = "municipality".}
\item{CD_GEOCMU}{Municipality identification code according to the Brazilian Institute of Geography and Statistics (Instituto Brasileiro de Geografia e Estatistica - IBGE). This variable will not appear if you choose the option level = "country" or level = "state".}
\item{country}{The country name. This variable will not appear if you choose the option level = "state" or level = "municipality".}
\item{FIRE_year}{The annual number of fire focus in the geographical level of interest between 2001 and 2019. These variables will not appear if you choose the option panel = TRUE.}
\item{year}{The value of the year. This variable will not appear if you choose the option panel = FALSE.}
\item{fires}{The annual number of fire focus in the geographical level of interest between 2001 and 2019. These variables will not appear if you choose the option panel = FALSE.}
}
\examples{
  my_df <- get_fires(level = "state", panel = FALSE)
}
\note{Please, cite: \url{https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms}}
