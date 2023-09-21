#' Scrapea los enlaces de enaho
#'
#' @param start_date year de inicio
#' @param end_date year final
#'
#' @return df de enlaces de modulo 3, 5, 85
#' @export
#'
#' @examples
#' scraping_enaho_datalink_function(start_date,end_date)
scraping_enaho_datalink_function=function(start_date,
                                          end_date){# 0. CONFIGURANDO EL SERVIDOR PARA SELENIUM

if (!"RSelenium" %in% installed.packages()[,"Package"]){install.packages("RSelenium")}
if (!"tidyverse" %in% installed.packages()[,"Package"]){install.packages("tidyverse")}
if (!"lubridate" %in% installed.packages()[,"Package"]){install.packages("lubridate")}
if (!"rvest" %in% installed.packages()[,"Package"]){install.packages("rvest")}

library(RSelenium)
library(tidyverse)
library(lubridate)
library(rvest)

#0. interest year

#start_date<-'2011'

#1. open browser

fdriver <- RSelenium::rsDriver(browser = c('chrome'),
                               port = 4444L,
                               chromever = 'latest', version="latest",
                               verbose = FALSE)

driver<-fdriver[['client']]

driver$maxWindowSize()

#2. microdata link

main_url<-'https://proyectos.inei.gob.pe/microdatos/'

driver$navigate(main_url)

Sys.sleep(runif(1,2,5))

open_query<-driver$findElement(using='xpath','//*[@id="jsmenu"]/li[1]/a')

open_query$clickElement()

Sys.sleep(runif(1,2,5))

# 2.1 access to enaho-actualizada poll

open_poll<-driver$findElement(using = 'xpath','//*[@id="select2-cmbEncuesta0ID-container"]')

open_poll$clickElement()

Sys.sleep(runif(1,2,5))

open_poll$sendKeysToActiveElement(list(key='down_arrow'))
open_poll$sendKeysToActiveElement(list(key='down_arrow'))
open_poll$sendKeysToActiveElement(list(key='enter'))

Sys.sleep(runif(1,2,5))

# 2.2 access to condiciones de vida y pobreza-ENAHO

open_enaho<-driver$findElement(using = 'xpath','//*[@id="ENAHON"]/select')

open_enaho$clickElement()

Sys.sleep(runif(1,2,5))

open_enaho$sendKeysToActiveElement(list(key='down_arrow'))
open_enaho$sendKeysToActiveElement(list(key='enter'))

Sys.sleep(runif(1,2,5))
# 2.3 access to years

page_to_year<-driver$getPageSource()[[1]]

temp_year<-read_html(page_to_year)%>%
  html_node('#divAnio')%>%
  html_nodes('option')%>%
  html_text()

interest_year<-which(str_detect(temp_year,start_date))
complete_year<-which(str_detect(temp_year,end_date))
# 2.4 access to trim for every year

links_year_temp<-list()

for (y in interest_year:complete_year) {

  #y=9

  open_year<-driver$findElement(using = 'xpath',
                                str_c('//*[@id="divAnio"]/select/option[',y,']'))

  open_year$clickElement()

  Sys.sleep(runif(1,2,5))

  page_to_trim<-driver$getPageSource()[[1]]

  temp_trim<-read_html(page_to_trim)%>%
    html_node('#divPeriodo')%>%
    html_nodes('option')%>%
    html_text()

  interest_trim<-which(temp_trim%>%
    str_detect('Trimestre'))

  links_trim_temp<-list()

  for (t in interest_trim) {

    #t=1

    message('scrapping links : ',str_c(temp_year[y],' - ',temp_trim[t]))



    open_trim<-driver$findElement('xpath',
                                  str_c('//*[@id="divPeriodo"]/select/option[',t,']'))
    open_trim$clickElement()

    Sys.sleep(runif(1,5,10))

    #2.5 scrapper to links

    page_to_scraper<-driver$getPageSource()[[1]]

    links_trim<-read_html(page_to_scraper)%>%
      html_node('#divDetalle')%>%
      html_nodes(xpath='//*[@id="divDetalle"]/table/tbody/tr/td/table/tbody/tr/td[9]')%>%
      html_nodes('a')%>%
      html_attr('href')%>%
      .[c(3,5,8)]

    # 2.7 as tibble

    links_trim_temp[[t-min(interest_trim)+1]]<-tibble(

      links=str_c('https://proyectos.inei.gob.pe',links_trim),
      period=str_c(temp_year[y],' - ',temp_trim[t])

      )

  }

  links_year_temp[[y-interest_year+1]]<-bind_rows(links_trim_temp)

}

driver$close()

all_link<-bind_rows(links_year_temp)

return(all_link)

}
