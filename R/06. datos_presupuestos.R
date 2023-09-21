#' scraping daos de ejecucion presupuestal
#'
#' @param start_date year de inicio
#' @param end_date year final
#'
#' @return df presupuestal
#' @export
#'
#' @examples
#' presupuesto_data(start_date,end_date)
presupuesto_data=function(start_date,end_date){
#() INSTALANDO PAQUETES Y CONFIGURANDO EL SERVIDOR PARA SELENIUM
if (!"RSelenium" %in% installed.packages()[,"Package"]){install.packages("RSelenium")}

library(RSelenium)
fdriver <- RSelenium::rsDriver(browser = c('chrome'),
                               port = 4444L,
                               chromever = 'latest', version="latest",
                               verbose = FALSE)

driver<-fdriver[['client']]



if (!"tidyverse" %in% installed.packages()[,"Package"]){install.packages("tidyverse")}
if (!"lubridate" %in% installed.packages()[,"Package"]){install.packages("lubridate")}
if (!"rvest" %in% installed.packages()[,"Package"]){install.packages("rvest")}
library(tidyverse)
library(lubridate)
library(rvest)

today<-year(today())

#start_date=2011
#end_date=2022

hist_years <- as.character(c(start_date:end_date))

departamento=seq_along(1:25)

hist_country=list()

for(d in departamento){

hist_departamento_repo <- list()

for(i in seq_along(hist_years)) {


  if(hist_years[i]==as.character(start_date)){
  #i=2

  driver$maxWindowSize()

  message(d,' - ',hist_years[i])

  #accedo a las paginas de interes

  url_1<-'https://apps5.mineco.gob.pe/transparencia/Navegador/default.aspx?y='

  url_2<-'&ap=ActProy'

  temp_year <-hist_years[i]

  temp_url_1 <-str_c(url_1, temp_year, url_2)


  driver$navigate(temp_url_1)

  #hay un marco dentro de la pagina, de manera que se requiere cambiar de nav

  #XML::htmlParse(driver$getPageSource()[[1]])

  Sys.sleep( runif(1, 1, 2) )

  frame_1<-driver$findElements(using = 'id','frame0')

  Sys.sleep( runif(1, 1, 2) )

  sapply(frame_1, function(x){x$getElementAttribute("src")})

  driver$switchToFrame(frame_1[[1]])

  #por lo tanto, una vez dentro del marco, procedo a navegar
  #entro a dar click en TOTAL y en la funcion del marco
  #para que aparezcan los datos que nos interesan

  gobierno<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnTipoGobierno"]')

  gobierno$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  locales<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_RptData_ctl02_TD0"]/input')

  locales$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  # gob_local<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnSubTipoGobierno"]')
  #
  # gob_local$clickElement()
  #
  # Sys.sleep( runif(1, 1, 3) )
  #
  # munis<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_RptData_ctl01_TD0"]/input')
  #
  # munis$clickElement()
  #
  # Sys.sleep( runif(1, 1, 3) )

  departamentos<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnDepartamento"]')

  departamentos$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  if(d<10){

  xpath=str_c('//*[@id="ctl00_CPH1_RptData_ctl',as.character(0),as.character(d),'_TD0"]/input')
  }else{
  xpath=str_c('//*[@id="ctl00_CPH1_RptData_ctl',as.character(d),'_TD0"]/input')
  }

  por_depart<-driver$findElement(using = 'xpath',xpath)

  por_depart$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  provincias<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnProvincia"]')

  provincias$clickElement()

  html_navegar<-driver$getPageSource()[[1]]

  #una vez descargado el html, se escrapea y se obtiene la tabla de presupuestos

  inputs<-read_html(html_navegar) %>%
    # Look for id="resultados"
    html_node('#PnlData') %>%
    # Look for anchor tags
    html_table(header=TRUE, fill = TRUE)

  #ubico las columnas de interes y los extraigo

  Devengado<-which(str_detect(inputs[1,],pattern = 'Devengado'))
  Avance<-which(str_detect(inputs[1,],pattern = 'Avance'))
  Provincia<-which(str_detect(inputs[1,],pattern = 'Provincia'))
  PIM<-which(str_detect(inputs[1,],pattern = 'PIM'))

  inputs_1<-inputs[,c(Provincia,PIM, Devengado, Avance)]

  #elimino las filas que no sirven, por suerte todas las fechas tendran
  #el mismo formato

  inputs_1<-inputs_1[-1,]%>%
    mutate(ubigeo=str_extract(Provincia,pattern = '[0-9][0-9][0-9][0-9]'))

  #renombro y depuro datos

  names(inputs_1)<-c('Entity', 'Income', 'Expenses', 'Avance','ubigeo')

  #inputs$ubigeo=as.character(str_extract(inputs_1$Entity,pattern = '[0-9][0-9][0-9][0-9]'))
  inputs_1$Entity<-str_remove_all(inputs_1$Entity,pattern = '[0-9]+[:]')%>%
    str_replace(' ','')
  inputs_1$Income<-as.numeric(str_remove_all(inputs_1$Income,pattern = ','))

  #final, esta parte es para almacenarlo en otra lista

  temp_df <-
    bind_cols(inputs_1
    ) %>%
    mutate( year = temp_year)
  temp_df

  hist_departamento_repo[[i]]<-temp_df

  }else{
  #i=2

  driver$maxWindowSize()

    message(d,' - ',hist_years[i])

  #accedo a las paginas de interes

  url_1<-'https://apps5.mineco.gob.pe/transparencia/Navegador/default.aspx?y='

  url_2<-'&ap=ActProy'

  temp_year <-hist_years[i]

  temp_url_1 <-str_c(url_1, temp_year, url_2)


  driver$navigate(temp_url_1)

  #hay un marco dentro de la pagina, de manera que se requiere cambiar de nav

  #XML::htmlParse(driver$getPageSource()[[1]])

  Sys.sleep( runif(1, 1, 2) )

  frame_1<-driver$findElements(using = 'id','frame0')

  Sys.sleep( runif(1, 1, 2) )

  sapply(frame_1, function(x){x$getElementAttribute("src")})

  driver$switchToFrame(frame_1[[1]])

  #por lo tanto, una vez dentro del marco, procedo a navegar
  #entro a dar click en TOTAL y en la funcion del marco
  #para que aparezcan los datos que nos interesan

  gobierno<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnTipoGobierno"]')

  gobierno$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  locales<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_RptData_ctl02_TD0"]/input')

  locales$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  gob_local<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnSubTipoGobierno"]')

  gob_local$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  munis<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_RptData_ctl01_TD0"]/input')

  munis$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  departamentos<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnDepartamento"]')

  departamentos$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  if(d<10){

    xpath=str_c('//*[@id="ctl00_CPH1_RptData_ctl',as.character(0),as.character(d),'_TD0"]/input')
  }else{
    xpath=str_c('//*[@id="ctl00_CPH1_RptData_ctl',as.character(d),'_TD0"]/input')
  }

  por_depart<-driver$findElement(using = 'xpath',xpath)

  por_depart$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  provincias<-driver$findElement(using = 'xpath','//*[@id="ctl00_CPH1_BtnProvincia"]')

  provincias$clickElement()

  html_navegar<-driver$getPageSource()[[1]]

  #una vez descargado el html, se escrapea y se obtiene la tabla de presupuestos

  inputs<-read_html(html_navegar) %>%
    # Look for id="resultados"
    html_node('#PnlData') %>%
    # Look for anchor tags
    html_table(header=TRUE, fill = TRUE)

  #ubico las columnas de interes y los extraigo

  Devengado<-which(str_detect(inputs[1,],pattern = 'Devengado'))
  Avance<-which(str_detect(inputs[1,],pattern = 'Avance'))
  Provincia<-which(str_detect(inputs[1,],pattern = 'Provincia'))
  PIM<-which(str_detect(inputs[1,],pattern = 'PIM'))

  inputs_1<-inputs[,c(Provincia,PIM, Devengado, Avance)]

  #elimino las filas que no sirven, por suerte todas las fechas tendran
  #el mismo formato

  inputs_1<-inputs_1[-1,]%>%
    mutate(ubigeo=str_extract(Provincia,pattern = '[0-9][0-9][0-9][0-9]'))

  #renombro y depuro datos

  names(inputs_1)<-c('Entity', 'Income', 'Expenses', 'Avance','ubigeo')

  #inputs$ubigeo=as.character(str_extract(inputs_1$Entity,pattern = '[0-9][0-9][0-9][0-9]'))
  inputs_1$Entity<-str_remove_all(inputs_1$Entity,pattern = '[0-9]+[:]')%>%
    str_replace(' ','')
  inputs_1$Income<-as.numeric(str_remove_all(inputs_1$Income,pattern = ','))

  #final, esta parte es para almacenarlo en otra lista

  temp_df <-
    bind_cols(inputs_1
    ) %>%
    mutate( year = temp_year)

  #temp_df

  hist_departamento_repo[[i]]<-temp_df

  }
}

hist_country[[d]]=bind_rows(hist_departamento_repo)

}



#hist_repo

hist_df <-bind_rows(hist_country)%>%
  select(year, everything())

#write_rds(hist_df,'avance_presupuestal.rds')

driver$close()

return(hist_df)

}

