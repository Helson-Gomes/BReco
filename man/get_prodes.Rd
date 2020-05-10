\name{get_prodes}
\alias{get_prodes}
\title{A function do download environmental data from brazilian Legal Amazon}
\usage{
get_prodes(year = NULL, state = NULL)
}
\description{
Download the data from the General Coordination for Earth Observation in Brazil (Coordenacao-Geral de Observacao da Terra - PRODES). The data contains information on the increase in deforestation, non-forest area, forest area, hydrography, cloud cover and deforestation area in the Brazilian Legal Amazon between 2002 and 2018.
}
\arguments{
\item{state}{A brazilian Legal Amazon state name abbreviation ("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO"). If you choose state = NULL, the function will return a table with information for all states in brazilian Legal Amazon.}
\item{year}{The year of interest. Choose an year between 2002 and 2018.}
}
\examples{
my_df <- get_prodes(year = 2018, state = NULL)
}
\value{
\item{Nr}{The number of each row in the data frame.}
\item{Lat}{The latitude coordinate.}
\item{Long}{The longitude coordinate.}
\item{Latgms}{Latitude coordinates in degrees, minutes and seconds.}
\item{Longms}{Longitude coordinates in degrees, minutes and seconds.}
\item{Municipio}{The name of each municipality.}
\item{CodIbge}{Municipality identification code according to the Brazilian Institute of Geography and Statistics (IBGE).}
\item{Estado}{The state name abbreviation. This variable will not appear if you choose the option level = "country" or level = "municipality".}
\item{AreaKm2}{The territorial area of the municipality in square kilometers.}
\item{Desmatado}{The deforested area in the year of interest in square kilometers.}
\item{Incremento}{The increase in the deforestation between the required year and the previous year in square kilometers.}
\item{Floresta}{The forest area in square kilometers.}
\item{Nuvem}{The average area covered by clouds.}
\item{NaoObservado}{Total excluded area due to excess of cloud in the year.}
\item{NaoFloresta}{The non-forest area in square kilometers.}
\item{Hidrografia}{Hydrography areas.}
\item{Soma}{Percentage of the monitored area in relation to the total area.}
}
\note{Pleace,cite: \url{http://www.dpi.inpe.br/prodesdigital/prodesmunicipal.php}.}
