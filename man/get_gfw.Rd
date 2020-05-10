\name{get_gfw}
\alias{get_gfw}
\title{A function to download data on C02 emissions, biomass loss and tree cover loss in Brazil.}
\usage{
  get_gfw(type_data = NULL, level = NULL)
}
\description{
  Download brazilian statistics on tree cover loss, CO2 emissions data based on aboveground biomass loss, and information on biomass loss in Brazil.
}
\arguments{
  \item{type_data}{The type of data required. Choose: type_data = "co2 emissions" if you want to download data about brazilian CO2 emissions;
  type_data = "biomass loss" if you want to download data about brazilian biomass loss;
  type_data = "tree cover loss" if you want to download data about tree cover loss of the brazilian forests.}
  \item{level}{The territorial level of interest . Choose: level = "Country" if you wannt to download brazilian agregate data;
  level = "Subnational 1" if you want to download data in state level;
  level = "Subnational 2" if you want to download data in municipal level.}
  }
  \value{
  \item{country}{The country name.}
  \item{threshold}{Percentage of canopy cover of interest.}
  \item{area_h}{The geographical area in hectares.}
  \item{extent_2000_ha}{The total covered area in hectares according to the Percentage of canopy cover of interest  in the year 2000.}
  \item{extent_2010_ha}{The total covered area in hectares according to the Percentage of canopy cover of interest  in the year 2010.}
  \item{co2_emissions_Mt_year}{Metric tonnes of CO2 emissions as a result of aboveground biomass loss at a required level, between 2001-2018, categorized by percent canopy cover.}
  \item{biomass_loss_Mt_year}{Metric tonnes of aboveground biomass loss at the required geographical level, between 2001-2018, categorized by percent canopy cover.}
  \item{tc_loss_ha_year}{Hectares of tree cover loss at the required geographical level, between 2001-2018, categorized by percent canopy cover.}
  }
\examples{
  my_df <- get_gfw(type_data = "co2 emissions", level = "Country")
}
\note{Pleace, cite:
  Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D.
  Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O.
  Justice, and J. R. G. Townshend. 2013. ???High-Resolution Global Maps of 21st-Century Forest
  Cover Change.??? Science 342 (15 November): 850???53. Data available on-line from:
  \url{http://earthenginepartners.appspot.com/science-2013-global-forest.}}
\note{Pleace, cite: Zarin, D., Harris, N.L. et al. 2016. Can carbon emissions drop by 50\% in five years? Global Change Biology, 22: 1336-1347. doi:10.1111/gcb.13153}
\note{Pleace, cite: Global Administrative Areas Database, version 3.6. Available at \url{http://gadm.org/}}

