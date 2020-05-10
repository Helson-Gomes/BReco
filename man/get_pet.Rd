\name{get_pet}
\alias{get_pet}
\title{A function to download data on potential average annual evapotranspiration in Brazil.}
\usage{
  get_pet(level = "municipality", panel = FALSE)
}
\description{
  Download data on brazilian annual potential evapotranspiration according to CAMARILLO-NARANJO et al (2019). Data are available at globalclimatemonitor.org.
  The output values refer to the geographical average of the values for each spatial unit (municipality, state or country).
}
\arguments{
  \item{level}{The geographic level of interest. This opition can be ("country", "state", or "municipality")}
  \item{panel}{Use panel = TRUE if you want to download the dataset in a panel data format and panel = FALSE otherwise. The default value is FALSE}
}
\value{
\item{YCNTRD}{The central longitude coordinate of the geographical area of interest.}
\item{XCNTRD}{The central latitude coordinate of the geographical area of interest.}
\item{CD_GEOCMU}{Municipality identification code according to the Brazilian Institute of Geography and Statistics (IBGE). This variable will not appear if you choose the option level = "country" or level = "state".}
\item{sigla.state}{The state name abbreviation. This variable will not appear if you choose the option level = "country" or level = "municipality".}
\item{code.state}{The state identification code according to the Brazilian Institute of Geography and Statistics (Instituto Brasileiro de Geografia e Estatistica - IBGE). This variable will not appear if you choose the option level = "country" or level = "municipality".}
\item{country}{The country name. This variable will not appear if you choose the option level = "state" or level = "municipality".}
\item{PET_year}{The annual average potential evapotranspiration of the geographical level required. These variables will not appear if you choose the option panel = TRUE.}
\item{evapotranspiration}{The annual average potential evapotranspiration of the geographical level required in panel data format. This variable will not appear if you choose the option panel = FALSE.}
\item{year}{The year of interest. This variable will not appear if you choose the option panel = FALSE.}
}
\examples{
  my_df <- get_pet(level = "state", panel = FALSE)
}
\note{Please, cite: \url{https://www.globalclimatemonitor.org/}}
\note{Please, cite: CAMARILLO-NARANJO, Juan Mariano et al. The global climate monitor system: from climate data-handling to knowledge dissemination. International journal of digital earth, v. 12, n. 4, p. 394-414, 2019.}
