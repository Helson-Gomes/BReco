#' Brazilian environmental data
#' @param year An year between 1970 and 2018
#' @param state A brazilian state code ('AC', 'AM', 'PA', 'RO', 'RR', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'ES', 'MG', 'SP', 'RJ', 'PR', 'SC', 'RS', 'MS', 'MT', 'GO')
#' @param activity An economical activity ('agriculture', 'energy', 'land use change', 'industry', 'waste')
#' @param gas The type of gas required

get_seeg <- function(state = NULL, activity = NULL, gas = NULL, type_data = NULL, panel = FALSE){
  # The code below help to avoid the note "no visible binding for global variable [variable name]"
  Estado <- setor <- Gas <- Emissao_Remocao <- year <- value <- NULL
  temp <- tempfile()
  if(is.null(activity) == FALSE){
    if(activity %in% c('agriculture', 'energy', 'land use change', 'industry', 'waste') == FALSE){
      stop('\n
        ------------------------------- \n
        Please, enter a valid activity! \n
        -------------------------------')
    }
  }
  if(is.null(state) == FALSE){
    if(state %in% c('AC', 'AM', 'PA', 'RO', 'RR', 'AP', 'TO', 'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'ES', 'MG', 'SP', 'RJ', 'PR', 'SC', 'RS', 'MS', 'MT', 'GO') == FALSE){
      stop('\n
      ------------------------------------------- \n
      Please, enter a valid brazilian state code! \n
      -------------------------------------------')
    }
  }
  if(is.null(gas) == FALSE){
    if (gas %in% c("CO2e (t) GTP-AR2", "CO2e (t) GTP-AR4", "CO2e (t) GTP-AR5", "CO2e (t) GWP-AR2",
                   "CO2e (t) GWP-AR4", "CO2e (t) GWP-AR5", "CO2 (t)", "CO (t)", "CH4 (t)", "NOx (t)", "N2O (t)", "COVNM (t)", "CF4 (t)", "C2F6 (t)",
                   "SF6 (t)", "HFC-23 (t)", "HFC-32 (t)", "HFC-134a (t)", "HFC-125 (t)", "HFC-143a (t)", "HFC-152a (t)", "NOX (t)" ) == FALSE){
      stop('\n
        -------------------------- \n
        Pleace, enter a valid gas! \n
        --------------------------')
    }
  }
  if(is.null(type_data) == FALSE){
    if(type_data %in% c('emissions', 'removal') == FALSE){
      stop('\n
      ------------------------------------------------------ \n
      Pleace, choose a opition between emissions or removal! \n
      ------------------------------------------------------')
    }
  }
  print('----- Wait for the data to download! -----')
  download.file('https://github.com/Helson-Gomes/dadostse/raw/master/SEEG.xlsx', temp, quiet = T)
  data <- readxl::read_excel(temp, 1)
  data <- dplyr::rename(data, setor = 1)
  if(is.null(state) == FALSE){
    data<- subset(data, Estado == state)
  }
  if(is.null(activity) == FALSE){
    if(activity == 'agriculture'){
      data <- subset(data, setor == 'Agropecuaria')
    }
    if(activity == 'energy'){
      data <- subset(data, setor == 'Energia')
    }
    if(activity == 'land use change'){
      data <- subset(data, setor == 'Mudanca de Uso da Terra')
    }
    if(activity == 'industry'){
      data <- subset(data, setor == 'Processos Industriais')
    }
    if(activity == 'waste'){
      data <- subset(data, setor == 'Residuos')
    }
  }
  if(is.null(gas) == FALSE){
    data <- subset(data, Gas == gas)
  }
  if(is.null(type_data) == FALSE){
    if (type_data == 'emissions'){
      data <- subset(data, Emissao_Remocao == "Emissao")
    }
    if (type_data == 'removal'){
      data <- subset(data, Emissao_Remocao == "Remocao")
    }
  }
  if(panel == TRUE){
    data <- tidyr::gather(data, year, value, '1970':'2018')
    data <- dplyr::select(data, 1,7,8,9,12,13)
  }
  if(panel == FALSE){
    data <- dplyr::select(data, 1, 7, 8, 9, 12:60)
  }
  if(panel %in% c(FALSE, TRUE) == FALSE){
    stop('\n
      --------------------------------------------- \n
      Please, choose panel = FALSE or panel = TRUE! \n
      ---------------------------------------------')
  }
  warning('\n
  ------------------------ \n
  Please, cite seeg.eco.br \n
  ------------------------')
  unlink(temp)
  return(data)
}

