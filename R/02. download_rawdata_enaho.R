#' descarga los .zip en la carpeta de archivos
#'
#' @param all_link requiere los enlaces escrapeados
#'
#' @return retorna los datos descargados para cada modulo
#' @export
#'
#' @examples
#' download_raw_enaho(all_link)
download_raw_enaho=function(all_link){#0. main directory and fixing your memory to download all data

memory.limit (20000)

library(tidyverse)
library(lubridate)
library(rvest)
library(haven)

#1. charge all_links

temp_raw_links<-all_link#scraping_enaho_datalink_function('2011','2022')

#2. download enaho data and setting download directory

enaho_data_mod03<-list()
enaho_data_mod05<-list()
enaho_data_mod85<-list()


setwd('archivos/')

if(length(list.files())>0){

  unlink(list.files(),recursive = TRUE)

}

for (l in 1:nrow(temp_raw_links)) {

  #l=sample(1:nrow(temp_raw_links),1)
  #l=70

  message('downloading: ',str_extract(temp_raw_links$links[l],
                                      '[A-z]+[0-9]+'),
          ' - ',temp_raw_links$period[l]
          )

  #2.1 download in .../archivos/

  file_name <- str_c('../archivos/',
                     str_extract(temp_raw_links$links[l],
                                            '[0-9][0-9][0-9]-[A-z]+[0-9]+'),
                                '.zip')


  download.file(url = temp_raw_links$links[l],destfile = file_name)

  # Sys.sleep(runif(1,3,5))

  #2.2 .sav inspect in every zip

  files_data <- untar(file_name, list=TRUE)

  list_p1<-files_data[which(files_data%>%
                              str_detect('Enaho'))]

  list_p<-list_p1[which(list_p1%>%
          str_detect('.sav')
          )]

  #2.3 paths located in every zip

  paths_located<-str_locate_all(list_p,'/')

  list_paths<-tibble(primero=str_sub(list_p[1],1,paths_located[[1]][1]-1),
                     segundo=str_sub(list_p[1],1,paths_located[[1]][2]-1)
  )

  #2.4 unzip

  unzip(str_extract(file_name,'[0-9][0-9][0-9]-[A-z]+[0-9]+.zip'))

  # Sys.sleep(runif(1,3,5))

  #3. cases:

  #3.1 if unzip fell .sav file into current path

  data<-list()

  if(dim(list_paths)[1]*dim(list_paths)[2]==sum(is.na(list_paths))){

    for (i in 1:length(list_p)) {

      data[[i]]<-read_sav(list_p[i])%>%
        mutate(
          period_mod=str_c(str_extract(file_name,'[A-z]+[0-9]+'),' - ', i ,
                           ' - ',
                           str_remove(temp_raw_links$period[l],'\n')%>%
                             str_trim())
        )

      names(data[[i]])[str_detect(names(data[[i]]),str_to_upper(str_c('\xf1')))]='ayo'

    }

    unlink(list.files(),recursive = TRUE)

  }

  #3.2 if unzip fell .sav file into one more consecutive path

  if(isTRUE(list_paths[1,1]==list_paths[1,2])){

    for (i in 1:length(list_p)) {

      data[[i]]<-read_sav(list_p[i])%>%
        mutate(
          period_mod=str_c(str_extract(file_name,'[A-z]+[0-9]+'),' - ', i ,
                           ' - ',
                           str_remove(temp_raw_links$period[l],'\n')%>%
                             str_trim())
        )

      names(data[[i]])[str_detect(names(data[[i]]),str_to_upper(str_c('\xf1')))]='ayo'

    }

    unlink(list.files(),recursive = TRUE)

  }

  #3.3 if unzip fell .sav file into two more consecutive path

  if(isTRUE(list_paths[1,1]!=list_paths[1,2])){

    for (i in 1:length(list_p)) {

      data[[i]]<-read_sav(list_p[i])%>%
        mutate(
          period_mod=str_c(str_extract(file_name,'[A-z]+[0-9]+'),' - ', i ,
                           ' - ',
                           str_remove(temp_raw_links$period[l],'\n')%>%
                             str_trim())
        )

      names(data[[i]])[str_detect(names(data[[i]]),str_to_upper(str_c('\xf1')))]='ayo'

    }

    unlink(list.files(),recursive = TRUE)

  }

  #3.4 saving the data by mods

  if(str_extract(temp_raw_links$links[l],'[A-z]+[0-9]+')=='Modulo03'){

    enaho_data_mod03[[length(enaho_data_mod03)+1]]<-data

  }

  if(str_extract(temp_raw_links$links[l],'[A-z]+[0-9]+')=='Modulo05'){

    enaho_data_mod05[[length(enaho_data_mod05)+1]]<-data

  }

  if(str_extract(temp_raw_links$links[l],'[A-z]+[0-9]+')=='Modulo85'){

    enaho_data_mod85[[length(enaho_data_mod85)+1]]<-data

  }

  # #3.5 pausing for bot
  #
  # Sys.sleep(runif(1,3,5))

}

#4. enaho_data structure:

#* 1. year
#* 2. trimestre
#* 3. modulo:
#*    3.1. Modulo03
#*    3.2. Modulo05
#*    3.3. Modulo85
#* 4. sub-modulos
#*    4.1. tipo1, i=1
#*    4.2. tipo2, i=2

#5. saving the enaho_raw_data

setwd('../')

message('saving data')


return(list(enaho_data_mod03=enaho_data_mod03,
            enaho_data_mod05=enaho_data_mod05,
            enaho_data_mod85=enaho_data_mod85))

}

