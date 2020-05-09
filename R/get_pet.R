#' Download data about brazilian anual mean evapotranspiration index according to CAMARILLO-NARANJO et al (2019)
#' @param level The territorial level required
#' @param panel Option to download data in panel data format

get_pet <- function(level = 'municipality', panel = FALSE){
  lapply(c("dplyr", "tidyr", "openxlsx"), require, character.only = TRUE)
  if(level == 'municipality'){
    print('Please, wait for the data to download!')
    dt <- read.xlsx('https://trello-attachments.s3.amazonaws.com/5ea83f462064047eed09f846/5ea83fc7d5707865983b4f14/12c62131af3179cbb4f21903ecd652b1/PET_1901_2012.xlsx')
  }
  if(level == 'state'){
    print('Please, wait for the data to download!')
    dt <- read.xlsx('https://trello-attachments.s3.amazonaws.com/5ea83f462064047eed09f846/5ea83fc7d5707865983b4f14/12c62131af3179cbb4f21903ecd652b1/PET_1901_2012.xlsx') %>%
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
      group_by(sigla.state, cod.state) %>% summarise_at(vars(4:115), funs(mean))
  }
  if(level == 'country'){
    print('Please, wait for the data to download!')
    dt <- read.xlsx('https://trello-attachments.s3.amazonaws.com/5ea83f462064047eed09f846/5ea83fc7d5707865983b4f14/12c62131af3179cbb4f21903ecd652b1/PET_1901_2012.xlsx') %>%
      mutate(country = 'Brazil') %>% group_by(country) %>%
      summarise_at(vars(4:115), funs(mean))
  }
  if(panel == TRUE){
    dt<-dt %>% gather(v1, evapotranspiration, 'PET_1901':'PET_2012') %>%
      mutate(year = substr(v1, 5,8)) %>% select(-v1)
  }
  if(level %in% c('municipality', 'state', 'country') == FALSE | panel %in% c(T, F) == FALSE){
    stop('\n
    -------------------------------------------------------- \n
    Please, enter a valid value to level or panel parameter! \n
    -------------------------------------------------------- \n')
  }
  warning('\n
  -------------------------------------------------- \n
  Please, cite https://www.globalclimatemonitor.org/ \n
  and \n
  CAMARILLO-NARANJO, Juan Mariano et al. The global \n
  climate monitor system: from climate data-handling \n
  to knowledge dissemination. International journal \n
  of digital earth, v. 12, n. 4, p. 394-414, 2019.
  -------------------------------------------------- ')
  return(dt)
}
