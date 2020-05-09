#' Download the data from the General Coordination for Earth Observation in Brazil (PRODES)
#' @param year The value of the year required
#' @param state The abbreviation for the name of the state of the Legal Amazon

get_prodes <- function(year = NULL, state = NULL){
  lapply(c("dplyr", "tidyr", "openxlsx"), require, character.only = TRUE)
  if(year %in% c(2002:2018) == FALSE){
    stop('\n
    -------------------------------------------- \n
    Please, choose a year between 2002 and 2018! \n
    --------------------------------------------')
  } else{
    dt <- read.csv2(paste(sprintf('http://www.dpi.inpe.br/prodesdigital/tabelatxt.php?ano=%i&estado=&ordem=DESMATAMENTO%i&type=tabela&output=txt&', year, year)), sep = ',', dec = '.', encoding = 'latin1')
    if(is.null(state) == FALSE){
      if(state %in% c('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO') == FALSE){
        stop('\n
        ----------------------------------------------- \n
        Please, choose a valid Legal Amazon state code! \n
        -----------------------------------------------')
      } else{
        dt <- dt %>%
          filter(Estado == state)
      }
    }
  }
  warning('\n
  --------------------------------------------------------------------- \n
  Please, cite http://www.dpi.inpe.br/prodesdigital/prodesmunicipal.php \n
  ---------------------------------------------------------------------')
  return(dt)
}
