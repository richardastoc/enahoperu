#' Eligiendo las variables de interes
#'
#' @param enaho_data datos de enaho
#' @param variables_xlsx las claves de las variables en excel
#' @param select_goals pre-funcion 1
#' @param binding_index pre-funcion 2
#' @param select_subset pre-funcion 3
#'
#' @return 1 df unido para cada modulo
#' @export
#'
#' @examples
#' selected_enaho_data(enaho_data,variables_xlsx,select_goals,binding_index,select_subset)
selected_enaho_data=function(enaho_data,
                             variables_xlsx,
                             select_goals, #1.funcion
                             binding_index,#2. funcion
                             select_subset){#3. funcion

  #0. fixing memory and required_packages (fixe your main working-path)

memory.limit (20000)

library(tidyverse)
library(lubridate)

#1. load enaho data by mods

#enaho_data=download_raw_enaho(a)

enaho_mod03<-enaho_data[1]$enaho_data_mod03#read_rds('enaho_raw_mod03.rds')
enaho_mod05<-enaho_data[2]$enaho_data_mod05#read_rds('enaho_raw_mod05.rds')
enaho_mod85<-enaho_data[3]$enaho_data_mod85#read_rds('enaho_raw_mod85.rds')

#2. load interest variables

# readxl::read_xlsx("VARIABLES.xlsx",
#                   'modxx')

#3. select interest variables to thesis by mods

variables_mod03<-readxl::read_xlsx(variables_xlsx,
                                   'mod03') %>%
  select(variable_mod03)%>%
  rename(variable=variable_mod03)

variables_mod05<-readxl::read_xlsx(variables_xlsx,
                                   'mod05') %>%
  select(variable_mod05)%>%
  rename(variable=variable_mod05)

variables_mod85<-readxl::read_xlsx(variables_xlsx,
                                   'mod85')  %>%
  select(variable_mod85)%>%
  rename(variable=variable_mod85)

#4. load functions

#source('03.1. select_&_biding_function.R')

################################################################################

#5. select variables by mods in enaho mod03 data

col_index_mod03<-list()
col_names_mod03<-list()
col_facto_mod03<-list()

index_names_facto_mod03<-select_goals(enaho_mod03,variables_mod03)

lngt<-length(enaho_mod03)

col_index_mod03[c(1:lngt)]    <-index_names_facto_mod03[c(1:lngt)]
col_names_mod03[c(1:lngt)]    <-index_names_facto_mod03[c((lngt+1):(2*lngt))]
col_facto_mod03[c(1:lngt)]    <-index_names_facto_mod03[c((2*lngt+1):(3*lngt))]

################################################################################

#6. select variables by mods in enaho mod05 data

col_index_mod05<-list()
col_names_mod05<-list()
col_facto_mod05<-list()

index_names_facto_mod05<-select_goals(enaho_mod05,variables_mod05)

col_index_mod05[c(1:lngt)]    <-index_names_facto_mod05[c(1:lngt)]
col_names_mod05[c(1:lngt)]    <-index_names_facto_mod05[c((lngt+1):(2*lngt))]
col_facto_mod05[c(1:lngt)]    <-index_names_facto_mod05[c((2*lngt+1):(3*lngt))]

################################################################################

#7. select variables by mods in enaho mod85 data

col_index_mod85<-list()
col_names_mod85<-list()
col_facto_mod85<-list()

index_names_facto_mod85<-select_goals(enaho_mod85,variables_mod85)

col_index_mod85[c(1:lngt)]    <-index_names_facto_mod85[c(1:lngt)]
col_names_mod85[c(1:lngt)]    <-index_names_facto_mod85[c((lngt+1):(2*lngt))]
col_facto_mod85[c(1:lngt)]    <-index_names_facto_mod85[c((2*lngt+1):(3*lngt))]

################################################################################

#7.x assessing balanced

balanced_03<-min(table(bind_rows(col_index_mod03)$name))
balanced_05<-min(table(bind_rows(col_index_mod05)$name))
balanced_85<-min(table(bind_rows(col_index_mod85)$name))

#falta para el P1$04 en el m?dulo 85

if(balanced_03==lngt & balanced_05==lngt & balanced_85==lngt){
  message('variables are balanced')
}else{
  message('variables aren\'t balanced | es normal')
}

#8. binding index for mods

#8.1. extracting general index:
#*    A. year
#*    B. MES
#*    C. CONGLOME
#*    D. VIVIENDA
#*    E. HOGAR
#*    F. CODPERSO
#*    G. UBIGEO

general_index_mod03<-list()
general_index_mod05<-list()
general_index_mod85<-list()

# lngt es el numero de trimestres de cualquier enaho
# si desea, cambiar por length(enaho_mod03) o cualquier modxx

for(y in 1:lngt){

  #y=28

  if(length(enaho_mod03[[y]])>1){

    list_mod03<-list()

  for(n in 1:2){

    list_mod03[[n]]<-tibble(index=c(1:7,length(enaho_mod03[[y]][[n]])),
                            name=names(enaho_mod03[[y]][[n]])[c(1:7,length(enaho_mod03[[y]][[n]]))]
                            )

  }
    general_index_mod03[[y]]<-list_mod03

  }else{

    general_index_mod03[[y]]<-tibble(index=c(1:7,length(enaho_mod03[[y]][[1]])),
                                     name=names(enaho_mod03[[y]][[1]])[c(1:7,length(enaho_mod03[[y]][[1]]))]
    )

  }

  if(length(enaho_mod85[[y]])>1){

    list_mod85<-list()

    for(n in 1:2){

    list_mod85[[n]]<-tibble(index=c(1:8,length(enaho_mod85[[y]][[n]])),
                            name=names(enaho_mod85[[y]][[n]])[c(1:8,length(enaho_mod85[[y]][[n]]))]
    )

    }

    general_index_mod85[[y]]<-list_mod85

  }else{

    general_index_mod85[[y]]<-tibble(index=c(1:8,length(enaho_mod85[[y]][[1]])),
                                     name=names(enaho_mod85[[y]][[1]])[c(1:8,length(enaho_mod85[[y]][[1]]))]
    )

  }


    general_index_mod05[[y]]<-tibble(index=c(1:9,length(enaho_mod05[[y]][[1]])),
                                     name=names(enaho_mod05[[y]][[1]])[c(1:9,length(enaho_mod05[[y]][[1]]))]
                                     )

}

#8.2. linking (binding function) the general index with the thesis variables index by mods

global_index_mod03<-binding_index(general_index_mod03,col_facto_mod03,col_index_mod03)
global_index_mod05<-binding_index(general_index_mod05,col_facto_mod05,col_index_mod05)
global_index_mod85<-binding_index(general_index_mod85,col_facto_mod85,col_index_mod85)

#9. apply index to mod using select_subset function

enaho_raw_mod03<-select_subset(enaho_mod03,global_index_mod03)
enaho_raw_mod05<-select_subset(enaho_mod05,global_index_mod05)
enaho_raw_mod85<-select_subset(enaho_mod85,global_index_mod85)


#10. obtaining data raw by interest variables by mods

enaho_bind_mod03<-bind_rows(enaho_raw_mod03)
enaho_bind_mod05<-bind_rows(enaho_raw_mod05)
enaho_bind_mod85<-bind_rows(enaho_raw_mod85)

#10.1. saving data raw by interest variables by mods

# write_rds(enaho_bind_mod03,'enaho_bind_mod03.rds')
# write_rds(enaho_bind_mod05,'enaho_bind_mod05.rds')
# write_rds(enaho_bind_mod85,'enaho_bind_mod85.rds')

return(list(enaho_bind_mod03=enaho_bind_mod03,
            enaho_bind_mod05=enaho_bind_mod05,
            enaho_bind_mod85=enaho_bind_mod85))

}
