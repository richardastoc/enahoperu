#' creando data panel para modulo 85
#'
#' @param enaho_mod85_df df del modulo
#'
#' @return df en formato panel
#' @export
#'
#' @examples
#' panel_data_85(enaho_mod85_df)
panel_data_85=function(enaho_mod85_df){#0. packages
memory.limit (2000000)

if (!"sjlabelled" %in% installed.packages()[,"Package"]){install.packages("sjlabelled")}
library(sjlabelled)
library(tidyverse)
library(lubridate)

#1. PANEL DATA ENAHO MOD85

#enaho_mod85_df<-read_rds('enaho_clean_mod85.rds')

#names(enaho_mod85_df)

#VARIABLES A CONSIDERAR:: necesitamos sumar anualmente antes de sacar proporciones

# 1.1. "blf.min.edu::confianza en el minedu"       1.2. "main.corrp"       1.3. "main.gob.credi"
# 1.4. "main.mply"       1.5. "edu.priority"

#1.1 variable: blf.min.edu #no lo uso

enaho_mod85_df1<-enaho_mod85_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,conf_muni)%>%
  summarise(
    tabla1=sum(factor85),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = conf_muni,
              values_from = tabla1)%>%
  ungroup()#%>%
  #select(-'NA')

enaho_mod85_df1[is.na(enaho_mod85_df1)]<-0

enaho_mod85_df1$total<-rowSums(enaho_mod85_df1[,3:length(enaho_mod85_df1)])

for (i in 3:(length(enaho_mod85_df1)-1)) {

  enaho_mod85_df1[,i]<-enaho_mod85_df1[,i]/enaho_mod85_df1[,length(enaho_mod85_df1)]
  colnames(enaho_mod85_df1)[i]<-str_c('conf_muni','-',names(enaho_mod85_df1[,i]))
}

enaho_mod85_df1<-enaho_mod85_df1%>%
  select(-total)

#1.2 variable: main.corrp #s
# hay dato faltante en 2011, pero
# se estudia a partir del 2012

enaho_mod85_df2<-enaho_mod85_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,main.corrp)%>%
  summarise(
    tabla1=sum(factor85),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = main.corrp,
              values_from = tabla1)%>%
  ungroup()

enaho_mod85_df2[is.na(enaho_mod85_df2)]<-0

enaho_mod85_df2$total<-rowSums(enaho_mod85_df2[,3:length(enaho_mod85_df2)])

for (i in 3:(length(enaho_mod85_df2)-1)) {

  enaho_mod85_df2[,i]<-enaho_mod85_df2[,i]/enaho_mod85_df2[,length(enaho_mod85_df2)]
  colnames(enaho_mod85_df2)[i]<-str_c('main.corrp','-',names(enaho_mod85_df2[,i]))
}

enaho_mod85_df2<-enaho_mod85_df2%>%
  select(-'main.corrp-Pase',-total)

#1.3 variable: main.gob.credi
# es una variable complicada.
# puede resolverse con dummys
# se corrigio en lo posible

enaho_mod85_df3<-enaho_mod85_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,main.gob.credi)%>%
  summarise(
    tabla1=sum(factor85),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = main.gob.credi,
              values_from = tabla1)%>%
  ungroup()%>%
  mutate(`la falta de credibilidad`=case_when(
    `la falta de credibilidad`!=0~`la falta de credibilidad`,
    TRUE~(lag(`la falta de credibilidad`,1)+lead(`la falta de credibilidad`,1))/2
  ))

enaho_mod85_df3[is.na(enaho_mod85_df3)]<-0

enaho_mod85_df3$total<-rowSums(enaho_mod85_df3[,3:length(enaho_mod85_df3)])

for (i in 3:(length(enaho_mod85_df3)-1)) {

  enaho_mod85_df3[,i]<-enaho_mod85_df3[,i]/enaho_mod85_df3[,length(enaho_mod85_df3)]
  colnames(enaho_mod85_df3)[i]<-str_c('main.gob.credi','-',names(enaho_mod85_df3[,i]))
}

enaho_mod85_df3<-enaho_mod85_df3%>%
  select(-'main.gob.credi-Pase',-total)

#1.4 variable: main.mply

enaho_mod85_df4<-enaho_mod85_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,main.mply)%>%
  summarise(
    tabla1=sum(factor85),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = main.mply,
              values_from = tabla1)%>%
  ungroup()

enaho_mod85_df4[is.na(enaho_mod85_df4)]<-0

enaho_mod85_df4$total<-rowSums(enaho_mod85_df4[,3:length(enaho_mod85_df4)])

for (i in 3:(length(enaho_mod85_df4)-1)) {

  enaho_mod85_df4[,i]<-enaho_mod85_df4[,i]/enaho_mod85_df4[,length(enaho_mod85_df4)]
  colnames(enaho_mod85_df4)[i]<-str_c('main.mply','-',names(enaho_mod85_df4[,i]))
}

enaho_mod85_df4<-enaho_mod85_df4%>%
  select(-'main.mply-Pase',-total)

#1.5 variable: edu.priority

enaho_mod85_df5<-enaho_mod85_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,edu.priority)%>%
  summarise(
    tabla1=sum(factor85),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = edu.priority,
              values_from = tabla1)%>%
  ungroup()

enaho_mod85_df5[is.na(enaho_mod85_df5)]<-0

enaho_mod85_df5$total<-rowSums(enaho_mod85_df5[,3:length(enaho_mod85_df5)])

for (i in 3:(length(enaho_mod85_df5)-1)) {

  enaho_mod85_df5[,i]<-enaho_mod85_df5[,i]/enaho_mod85_df5[,length(enaho_mod85_df5)]
  colnames(enaho_mod85_df5)[i]<-str_c('edu.priority','-',names(enaho_mod85_df5[,i]))
}

enaho_mod85_df5<-enaho_mod85_df5%>%
  select(-'edu.priority-Pase',-total)



#1.7 assessing mod85 data

cnt<- length(enaho_mod85_df1$ubigeo)+
  length(enaho_mod85_df2$ubigeo)+
  length(enaho_mod85_df3$ubigeo)+
  length(enaho_mod85_df4$ubigeo)+
  length(enaho_mod85_df5$ubigeo)

if(cnt==5*length(enaho_mod85_df1$ubigeo)){
  message('mod85 panel data is balanced')
}

#1.8. biding mod85 data

enaho_panel_mod85<-enaho_mod85_df1%>%
  left_join(enaho_mod85_df2,by=c('ubigeo','year'))%>%
  left_join(enaho_mod85_df3,by=c('ubigeo','year'))%>%
  left_join(enaho_mod85_df4,by=c('ubigeo','year'))%>%
  left_join(enaho_mod85_df5,by=c('ubigeo','year'))

#1.9. saving mod85 panel data

#write_rds(enaho_panel_mod85,'enaho_panel_mod85.rds')

return(enaho_panel_mod85)
}
