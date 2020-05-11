\name{get_seeg}
\alias{get_seeg}
\title{A function to download data on greenhouse gas emissions in Brazil}
\usage{
get_seeg(state = NULL, activity = NULL, gas = NULL, type_data = NULL, panel = FALSE)
}
\description{
Download data from Greenhouse Gas Emissions and Removal Estimation System ("Sistema de Estimativas de Emisscoes e Remocoes de Gases de Efeito Estufa") - SEEG - available in "seeg.eco.br".
}
\details{If you choose panel = FALSE, the value of the greenhouse gas emissions or removals will be returned in columns named with the number of each year.}
\arguments{
\item{state}{A brazilian state code ("AC", "AM", "PA", "RO", "RR", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "ES", "MG", "SP", "RJ", "PR", "SC", "RS", "MS", "MT", "GO")}
\item{activity}{An economical activity ("agriculture", "energy", "land use change", "industry", "waste")}
\item{gas}{The type of gas of interest ("CO2e (t) GTP-AR2", "CO2e (t) GTP-AR4", "CO2e (t) GTP-AR5", "CO2e (t) GWP-AR2",
"CO2e (t) GWP-AR4", "CO2e (t) GWP-AR5", "CO2 (t)", "CO (t)", "CH4 (t)", "NOx (t)", "N2O (t)", "COVNM (t)", "CF4 (t)", "C2F6 (t)",
"SF6 (t)", "HFC-23 (t)", "HFC-32 (t)", "HFC-134a (t)", "HFC-125 (t)", "HFC-143a (t)", "HFC-152a (t)", "NOX (t)" }
\item{type_data}{Use type = "emissions" if you want to download data on gas emissions and type = "removal" if you want to download data on gas removals.}
\item{panel}{Use panel = TRUE if you want to download the database in a panel data format and panel = FALSE otherwise.}
}
\value{
\item{setor}{The economic activity of interest.}
\item{Emissao_Remocao}{Returns "Emissao" to emissions and "Remocao" to removal.}
\item{Gas}{The type of gas.}
\item{Estado}{The state name abbreviation.}
\item{year}{The value of the year. This variable will not appear if you choose the option panel = FALSE.}
\item{value}{The value of greenhouse gas emissions or removals. This variable will not appear if you choose the option panel = FALSE.}
}
\examples{
my_df <- get_seeg(state = NULL, activity = NULL, type_data = NULL, panel = TRUE)
}
\note{Please, cite: seeg.eco.br.}
