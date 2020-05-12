get_prodes <- function(year = NULL, state = NULL){
  # The code below help to avoid the note "no visible binding for global variable [variable name]"
  Estado <- NULL
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
        dt <- dplyr::filter(dt, Estado == state)
      }
    }
  }
  warning('\n
  --------------------------------------------------------------------- \n
  Please, cite http://www.dpi.inpe.br/prodesdigital/prodesmunicipal.php \n
  ---------------------------------------------------------------------')
  return(dt)
}


