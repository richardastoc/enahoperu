#' Title extrae los datos de poblacion provincial
#'
#' @param start_date periodo inicial
#' @param end_date periodo final
#' @param tildes string de tildes en mayuscula
#'
#' @return el df de poblacion
#' @export
#'
#' @examples
#' poblacion_data(start_date,end_date,tildes)
poblacion_data=function(start_date,end_date,tildes){
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

  driver$maxWindowSize()

  #datos excepto 2016 y 2017

  url_1<-'https://systems.inei.gob.pe/SIRTOD/app/consulta'

  driver$navigate(url_1)

  Sys.sleep(10)

  page_to_ubigeo<-driver$getPageSource()[[1]]

  temp_provincias<-read_html(page_to_ubigeo)%>%
    html_node(xpath='//*[@id="ubigeo-provincial-listado"]')%>%
    html_nodes('label')%>%
    html_text()%>%.[-1]

  ub_ini=str_locate_all(temp_provincias,'-')
  ub_end=str_locate_all(temp_provincias,'/')

  name_raw=list()


  for (i in 1:length(temp_provincias)) {
    #i=1
    ub_ini=str_locate_all(temp_provincias[i],'-')[[1]][1,1]-2
    ub_end=str_locate_all(temp_provincias[i],'/')[[1]][1,1]+2
    temp_name1=str_sub(temp_provincias[i],1,ub_ini)
    temp_name2=str_sub(temp_provincias[i],ub_end,str_length(temp_provincias[i]))
    name_raw[[i]]=tibble(ubigeo=temp_name1,
                         provincia=chartr(tildes, "AEIOU", toupper(temp_name2))
                           )
  }

  ubigeo_and_name=bind_rows(name_raw)

  #driver$navigate(url_1)

  provincia<-driver$findElement(using = 'id',
                                'ubigeo-provincial-buscador')

  provincia$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  todos<-driver$findElement(using = 'id',
                                'ubigeo-agregartodos-prov')

  todos$clickElement()

  todos$sendKeysToActiveElement(list(key='enter'))


  auxiliar<-driver$findElements(using = 'id',
                                'ubigeo-ejecutar')

  driver$mouseMoveToLocation(webElement = auxiliar[[1]])

  Sys.sleep( runif(1, 1, 2) )

  demografi<-driver$findElement(using = 'id',
                            'nodop16815')

  demografi$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  proyectada<-driver$findElement(using = 'id',
                                'nodop41622')

  proyectada$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  total_proyectada<-driver$findElement(using = 'id',
                                 'nodop41623')

  total_proyectada$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  total_proyectada_click<-driver$findElement(using = 'id',
                                       'nodoh41624')

  total_proyectada_click$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  ejecutar<-driver$findElement(using = 'id',
                                             'ubigeo-ejecutar')

  ejecutar$clickElement()

  Sys.sleep(10)

  page_to_table<-driver$getPageSource()[[1]]

  temp_poblacion<-read_html(page_to_table)%>%
    html_node('#pvtTable')%>%
    html_table()%>%
    .[-1,-3]

  names(temp_poblacion)[c(1,2)]=c('departamento','provincia')

  temp_poblacion_df=temp_poblacion%>%
    select(-Totales)%>%
    .[-c(197,198),]


  data_raw=pivot_longer(temp_poblacion_df,
                        !c(departamento,provincia),
                        names_to = 'year',
                        values_to = 'POB')

  data_raw$POB= as.numeric(str_remove_all(data_raw$POB,' '))

  data_raw$provincia= chartr(tildes, "AEIOU", toupper(data_raw$provincia))

  data_raw_pob=data_raw%>%
    left_join(ubigeo_and_name,
              by='provincia')%>%
    mutate(POB=log(POB))

  # 2. datos de 2016

  tasa_2016=data_raw_pob%>%filter(year>='2007',year<='2015')%>%
    mutate(tasa=POB-lag(POB,1))%>%
    filter(year>'2007')%>%
    group_by(departamento,provincia)%>%
    summarise(tasa_m=mean(tasa))

  data_2015=data_raw_pob[data_raw_pob$year=='2015',]%>%
    left_join(tasa_2016,
              by=c('departamento','provincia'))

  data_raw_2016=list()

  for (j in 1:dim(data_2015)[1]) {
    #j=1
    data_raw_2016[[j]]=tibble(
      data_2015[j,1],
      data_2015[j,2],
      year='2016',
      log(exp(data_2015[j,4])*(1+data_2015[j,6])),
      data_2015[j,5]
    )
  }

  data_df_2016=bind_rows(data_raw_2016)

  pob_raw=data_raw_pob%>%full_join(data_df_2016)

  # 3. datos del 2017

  driver$navigate(url_1)

  Sys.sleep(10)

  page_to_ubigeo<-driver$getPageSource()[[1]]

  temp_provincias2<-read_html(page_to_ubigeo)%>%
    html_node(xpath='//*[@id="ubigeo-provincial-listado"]')%>%
    html_nodes('label')%>%
    html_text()%>%.[-1]

  ub_ini2=str_locate_all(temp_provincias,'-')
  ub_end2=str_locate_all(temp_provincias,'/')

  name_raw2=list()

  for (i in 1:length(temp_provincias2)) {
    #i=1
    ub_ini2=str_locate_all(temp_provincias2[i],'-')[[1]][1,1]-2
    ub_end2=str_locate_all(temp_provincias2[i],'/')[[1]][1,1]+2
    temp_namex=str_sub(temp_provincias2[i],1,ub_ini2)
    temp_namey=str_sub(temp_provincias2[i],ub_end2,str_length(temp_provincias2[i]))
    name_raw2[[i]]=tibble(ubigeo=temp_namex,
                         provincia=chartr(tildes, "AEIOU", toupper(temp_namey))
    )
  }

  ubigeo_and_name2=bind_rows(name_raw2)

  #driver$navigate(url_1)

  provincia<-driver$findElement(using = 'id',
                                'ubigeo-provincial-buscador')

  provincia$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  todos<-driver$findElement(using = 'id',
                            'ubigeo-agregartodos-prov')

  todos$clickElement()

  todos$sendKeysToActiveElement(list(key='enter'))


  auxiliar<-driver$findElements(using = 'id',
                                'ubigeo-ejecutar')

  driver$mouseMoveToLocation(webElement = auxiliar[[1]])

  Sys.sleep( runif(1, 1, 2) )

  demografi<-driver$findElement(using = 'id',
                                'nodop16815')

  demografi$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  poblacion_total<-driver$findElement(using = 'id',
                                 'nodop437590')

  poblacion_total$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  poblacion_total_i<-driver$findElement(using = 'id',
                                       'nodoh437673')

  poblacion_total_i$clickElement()

  Sys.sleep( runif(1, 1, 2) )

  ejecutar<-driver$findElement(using = 'id',
                               'ubigeo-ejecutar')

  ejecutar$clickElement()

  Sys.sleep(10)

  page_to_table2<-driver$getPageSource()[[1]]

  temp_poblacion_17<-read_html(page_to_table2)%>%
    html_node('#pvtTable')%>%
    html_table()%>%
    .[-1,-3]

  names(temp_poblacion_17)[c(1,2)]=c('departamento','provincia')

  temp_poblacion17_df=temp_poblacion_17%>%
    select(-Totales)%>%
    .[-c(197,198),]


  data_raw17=pivot_longer(temp_poblacion17_df,
                        !c(departamento,provincia),
                        names_to = 'year',
                        values_to = 'POB')

  data_raw17$POB= log(as.numeric(str_remove_all(data_raw17$POB,' ')))

  pob_df=pob_raw%>%full_join(data_raw17)%>%
    arrange(year)%>%
    filter(year<=as.character(end_date),year>=as.character(start_date))%>%
    mutate(provincia_pob=provincia)

  driver$close()

  return(pob_df)
}
