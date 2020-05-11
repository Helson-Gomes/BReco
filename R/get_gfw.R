#' Download brasilian statistics about tree cover loss, CO2 emissions data based on aboveground biomass loss, and biomass loss informations available in "https://www.globalforestwatch.org/".
#' @param type_data The type of data required
#' @param level The territorial level required

get_gfw <- function(type_data = NULL, level = NULL){
  utils::globalVariables()
  temp <- tempfile()
  if(is.null(type_data) == FALSE){
    if(is.null(level) == FALSE){
      if(level %in% c('Country', 'Subnational 1', 'Subnational 2') == FALSE){
        stop('
        ----------------------------------------- \n
        Please, choose a valid territorial level! \n
        -----------------------------------------')
      }
    } else{
      stop('
      ----------------------------------------- \n
      Please, choose a valid territorial level! \n
      -----------------------------------------')
    }
    if(type_data %in% c('co2 emissions', 'biomass loss', 'tree cover loss') == FALSE){
      stop('
      --------------------------------- \n
      Please, choose a valid data type! \n
      --------------------------------')
    }
    print('Please, wait for the data to download!')
    download.file('https://gfw2-data.s3.amazonaws.com/country-pages/country_stats/download/BRA.xlsx', temp, quiet = T)
    df <- readxl::read_excel(temp, sheet = paste(level, type_data))


  }else{
    stop('\n
    ---------------------------------- \n
    Please, choose a valide data type! \n
    ----------------------------------')
  }
  unlink(temp)
  warning('\n
  ----------------------------------------------------------------------------------------------------\n
  Pleace, cite: \n
  Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D.\n
  Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. \n
  Justice, and J. R. G. Townshend. 2013. ???High-Resolution Global Maps of 21st-Century Forest \n
  Cover Change.??? Science 342 (15 November): 850???53. Data available on-line from: \n
  http://earthenginepartners.appspot.com/science-2013-global-forest. \n
  \n
  Zarin, D., Harris, N.L. et al. 2016. Can carbon emissions drop by 50% in five years? Global \n
  Change Biology, 22: 1336-1347. doi:10.1111/gcb.13153 \n
  and \n
  \n
  Global Administrative Areas Database, version 3.6. Available at http://gadm.org/ \n
  ----------------------------------------------------------------------------------------------------')
  return(df)
}


