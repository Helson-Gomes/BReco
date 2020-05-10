\name{get_rainfall}
\alias{get_rainfall}
\title{A function to download data on annual average rainfall in Brazil}
\usage{
  get_rainfall(level = "municipality", panel = FALSE)
}
\description{
  Download data on brazilian annual average rainfall according to CAMARILLO-NARANJO et al (2019). Data are available at "globalclimatemonitor.org".
  The output values refer to the geographical average of the values for each spatial unit (municipality, state or country)}
\arguments{
  \item{level}{The geographic level of interest. This opition can be ("country", "state", or "municipality")}
  \item{panel}{Use panel = TRUE if you want to download the database in a panel data format and panel = FALSE otherwise. The default value is FALSE}
}
\value{
\item{YCNTRD}{The central longitude coordinate of the geographical area of interest.}
\item{XCNTRD}{The central latitude coordinate of the geographical area of interest.}
\item{CD_GEOCMU}{Municipality identification code according to the Brazilian Institute of Geography and Statistics (IBGE). This variable will not appear if you choose the option level = "country" or level = "state".}
\item{sigla.state}{The state name abbreviation. This variable will not appear if you choose the option level = "country" or level = "municipality".}
\item{code.state}{The state identification code according to the Brazilian Institute of Geography and Statistics (Instituto Brasileiro de Geografia e Estatistica - IBGE). This variable will not appear if you choose the option level = "country" or level = "municipality".}
\item{NM_MUNICIP}{The name of each municipality. This variable not appear if you choose the option level = "state" or level = "country".}
\item{country}{The country name. This variable will not appear if you choose the option level = "state" or level = "municipality".}
\item{PREC_year}{The annual average rainfall (mm) between 1901 and 2019. These variables will not appear if you choose the option level = "state" or level = "country".}
\item{rainfall}{The annual average rainfall (mm) when you choose the option panel = TRUE.}
}
\examples{
  my_df <- get_rainfall(level = "state", panel = FALSE)
}
\note{Please, cite \url{https://www.globalclimatemonitor.org/}.}
\note{Please, cite CAMARILLO-NARANJO, Juan Mariano et al. The global climate monitor system: from climate data-handling to knowledge dissemination. International journal of digital earth, v. 12, n. 4, p. 394-414, 2019.}
