#' df total
#'
#' @param enaho_panel_data df de enaho
#' @param hist_df df de ejecucion
#' @param pob_df
#'
#' @return df conjunto
#' @export
#'
#' @examples
#' al_data(enaho_panel_data,hist_df,pob_df)
al_data=function(enaho_panel_data,
                 hist_df,
                 pob_df){#0. packages
memory.limit (2000000)

if (!"sjlabelled" %in% installed.packages()[,"Package"]){install.packages("sjlabelled")}
library(sjlabelled)
library(tidyverse)
library(lubridate)

enaho<-enaho_panel_data
mef<-hist_df%>%
  mutate(provincia_mef=case_when(
    Entity=='VILCASHUAMAN'~'VILCAS HUAMAN',
    TRUE~Entity
  ))%>%
  select(-Entity)%>%
  arrange(provincia)

df_country<-enaho%>%
  left_join(mef,
            by=c('ubigeo','year'))%>%
  left_join(pob_df,
            by=c('ubigeo','year'))


#write_rds(df_provincias,'df_provincias.rds')

return(df_country)

}
