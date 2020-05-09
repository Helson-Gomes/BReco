#' Download data from Fire Information for Resource Management System (FIRMS) to Brazil
#' @param level The territorial level required
#' @param panel Option to download data in panel data formt

get_fires <- function(level = NULL, panel = FALSE){
  lapply(c("dplyr", "tidyr", "openxlsx"), require, character.only = TRUE)
  if(is.null(level) == TRUE){
    dt <- read.xlsx('https://trello-attachments.s3.amazonaws.com/5ea83f462064047eed09f846/5ea83fc7d5707865983b4f14/16378a8eeeb8d1651493fe4e9606cf51/INCENDIOS_2001_2019.xlsx')
  }else{
    if(level == 'municipality'){
      dt <-read.xlsx('https://trello-attachments.s3.amazonaws.com/5ea83f462064047eed09f846/5ea83fc7d5707865983b4f14/16378a8eeeb8d1651493fe4e9606cf51/INCENDIOS_2001_2019.xlsx')
    }
    if(level == 'state'){
      dt <- read.xlsx('https://trello-attachments.s3.amazonaws.com/5ea83f462064047eed09f846/5ea83fc7d5707865983b4f14/16378a8eeeb8d1651493fe4e9606cf51/INCENDIOS_2001_2019.xlsx') %>%
        mutate(cod.state = substr(CD_GEOCMU, 1, 2),
               sigla.state = ifelse(cod.state == '11', 'RO' , 'DF'),
               sigla.state = ifelse(cod.state == '12', 'AC' , sigla.state), sigla.state = ifelse(cod.state == '13', 'AM' , sigla.state),
               sigla.state = ifelse(cod.state == '14', 'RR' , sigla.state), sigla.state = ifelse(cod.state == '15', 'PA' , sigla.state),
               sigla.state = ifelse(cod.state == '16', 'AP' , sigla.state), sigla.state = ifelse(cod.state == '17', 'TO' , sigla.state),
               sigla.state = ifelse(cod.state == '21', 'MA' , sigla.state), sigla.state = ifelse(cod.state == '22', 'PI' , sigla.state),
               sigla.state = ifelse(cod.state == '23', 'CE' , sigla.state), sigla.state = ifelse(cod.state == '24', 'RN' , sigla.state),
               sigla.state = ifelse(cod.state == '25', 'PB' , sigla.state), sigla.state = ifelse(cod.state == '26', 'PE' , sigla.state),
               sigla.state = ifelse(cod.state == '27', 'AL' , sigla.state), sigla.state = ifelse(cod.state == '28', 'SE' , sigla.state),
               sigla.state = ifelse(cod.state == '29', 'BA' , sigla.state), sigla.state = ifelse(cod.state == '31', 'MG' , sigla.state),
               sigla.state = ifelse(cod.state == '32', 'ES' , sigla.state), sigla.state = ifelse(cod.state == '33', 'RJ' , sigla.state),
               sigla.state = ifelse(cod.state == '35', 'SP' , sigla.state), sigla.state = ifelse(cod.state == '41', 'PR' , sigla.state),
               sigla.state = ifelse(cod.state == '42', 'SC' , sigla.state), sigla.state = ifelse(cod.state == '43', 'RS' , sigla.state),
               sigla.state = ifelse(cod.state == '50', 'MS' , sigla.state), sigla.state = ifelse(cod.state == '51', 'MT' , sigla.state),
               sigla.state = ifelse(cod.state == '52', 'GO' , sigla.state)) %>%
        group_by(sigla.state) %>% summarise_at(vars(2:20), funs(sum))
    }
    if(level == 'country'){
      df<-df %>% mutate(country = 'Brazil') %>% group_by(country) %>%
        summarise_at(vars(2:20), funs(sum))
    }
  }
  if(panel == TRUE){
    dt<-dt %>% gather(v1, fires, 'FIRE_2001':'FIRE_2019') %>%
      mutate(year = substr(v1, 6, 9)) %>% select(-v1)
  }
  warning('\n
  -------------------------------------------------------------------------------------- \n
  Please, cite https://earthdata.nasa.gov/earth-observation-data/near-real-time/firms \n
  -------------------------------------------------------------------------------------- ')
  return(dt)
}
