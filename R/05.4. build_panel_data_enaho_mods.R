#' Unificando todos los modulos
#'
#' @param enaho_panel_mod03 modulo 3
#' @param enaho_panel_mod05 modulo 5
#' @param enaho_panel_mod85 modulo 85
#'
#' @return retorna un data panel para todas las variables
#' @export
#'
#' @examples
#' enaho_panel_data(enaho_panel_mod03,enaho_panel_mod05,enaho_panel_mod85)
enaho_panel_data=function(enaho_panel_mod03,
                    enaho_panel_mod05,
                    enaho_panel_mod85){
#0. packages
memory.limit (2000000)

if (!"sjlabelled" %in% installed.packages()[,"Package"]){install.packages("sjlabelled")}
library(sjlabelled)
library(tidyverse)
library(lubridate)

#1. PANEL DATA ENAHO all MODS

enaho_pmod03_df<-enaho_panel_mod03
enaho_pmod05_df<-enaho_panel_mod05
enaho_pmod85_df<-enaho_panel_mod85

enaho_panel_data<-enaho_pmod03_df%>%
  left_join(enaho_pmod05_df,
            by=c('ubigeo','year'))%>%
  left_join(enaho_pmod85_df,
            by=c('ubigeo','year'))

#2. saving global data

#write_rds(enaho_panel_data,'enaho_mods_data.rds')
#write.csv(enaho_panel_data,'enaho_mods_data.csv')

return(enaho_panel_data)
}
