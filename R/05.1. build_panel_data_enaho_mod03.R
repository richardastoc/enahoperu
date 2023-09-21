#' creando panel para modulo 3
#'
#' @param enaho_mod03_df df del modulo
#'
#' @return df en data panel
#' @export
#'
#' @examples
#' panel_data_03(enaho_mod03_df)
panel_data_03=function(enaho_mod03_df){
#0. packages
memory.limit (2000000)

if (!"sjlabelled" %in% installed.packages()[,"Package"]){install.packages("sjlabelled")}
library(sjlabelled)
library(tidyverse)
library(lubridate)

#1. PANEL DATA ENAHO MOD03
#enaho_mod03_df<-read_rds('enaho_clean_mod03.rds')

#names(enaho_mod03_df)

   #VARIABLES A CONSIDERAR

   # 1.1. "N.REdu:nivel de educacion"       1.2. "C.REst::centro de estudio"       1.3. "N.CEdu::sigue estudiando"
   # 1.4. "C.CEst::centro de estudio al q asiste"       1.5. "Capac::capacitaciones"        1.6. "Inter::internet"

#1.1 variable: N.REdu

enaho_mod03_df1<-enaho_mod03_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,N.REdu)%>%
  summarise(
    tabla1=sum(factor03),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = N.REdu,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod03_df1[is.na(enaho_mod03_df1)]<-0

enaho_mod03_df1$total<-rowSums(enaho_mod03_df1[,3:length(enaho_mod03_df1)])

for (i in 3:(length(enaho_mod03_df1)-1)) {

  enaho_mod03_df1[,i]<-enaho_mod03_df1[,i]/enaho_mod03_df1[,length(enaho_mod03_df1)]
  colnames(enaho_mod03_df1)[i]<-str_c('N.REdu','-',names(enaho_mod03_df1[,i]))
}

enaho_mod03_df1<-enaho_mod03_df1%>%
  select(-total)

#1.2 variable: C.REst #no no uso, hay problemas

enaho_mod03_df2<-enaho_mod03_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,C.REst)%>%
  summarise(
    tabla1=sum(factor03),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = C.REst,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod03_df2[is.na(enaho_mod03_df2)]<-0

enaho_mod03_df2$total<-rowSums(enaho_mod03_df2[,3:length(enaho_mod03_df2)])

for (i in 3:(length(enaho_mod03_df2)-1)) {

  enaho_mod03_df2[,i]<-enaho_mod03_df2[,i]/enaho_mod03_df2[,length(enaho_mod03_df2)]
  colnames(enaho_mod03_df2)[i]<-str_c('C.REst','-',names(enaho_mod03_df2[,i]))
}

enaho_mod03_df2<-enaho_mod03_df2%>%
  select(-total)

#1.3 variable: N.CEdu #tampoco lo uso

enaho_mod03_df3<-enaho_mod03_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,N.CEdu)%>%
  summarise(
    tabla1=sum(factor03),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = N.CEdu,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod03_df3[is.na(enaho_mod03_df3)]<-0

enaho_mod03_df3$total<-rowSums(enaho_mod03_df3[,3:length(enaho_mod03_df3)])

for (i in 3:(length(enaho_mod03_df3)-1)) {

  enaho_mod03_df3[,i]<-enaho_mod03_df3[,i]/enaho_mod03_df3[,length(enaho_mod03_df3)]
  colnames(enaho_mod03_df3)[i]<-str_c('N.CEdu','-',names(enaho_mod03_df3[,i]))
}

enaho_mod03_df3<-enaho_mod03_df3%>%
  select(-total)

#1.4 variable: C.CEst #tampoco lo uso

enaho_mod03_df4<-enaho_mod03_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,C.CEst)%>%
  summarise(
    tabla1=sum(factor03),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = C.CEst,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod03_df4[is.na(enaho_mod03_df4)]<-0

enaho_mod03_df4$total<-rowSums(enaho_mod03_df4[,3:length(enaho_mod03_df4)])

for (i in 3:(length(enaho_mod03_df4)-1)) {

  enaho_mod03_df4[,i]<-enaho_mod03_df4[,i]/enaho_mod03_df4[,length(enaho_mod03_df4)]
  colnames(enaho_mod03_df4)[i]<-str_c('C.CEst','-',names(enaho_mod03_df4[,i]))
}

enaho_mod03_df4<-enaho_mod03_df4%>%
  select(-total)

#1.5 variable: Capac #correcto

enaho_mod03_df5<-enaho_mod03_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,Capac)%>%
  summarise(
    tabla1=sum(factor03),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = Capac,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod03_df5[is.na(enaho_mod03_df5)]<-0

enaho_mod03_df5$total<-rowSums(enaho_mod03_df5[,3:length(enaho_mod03_df5)])

for (i in 3:(length(enaho_mod03_df5)-1)) {

  enaho_mod03_df5[,i]<-enaho_mod03_df5[,i]/enaho_mod03_df5[,length(enaho_mod03_df5)]
  colnames(enaho_mod03_df5)[i]<-str_c('capac','-',names(enaho_mod03_df5[,i]))
}

enaho_mod03_df5<-enaho_mod03_df5%>%
  select(-total)

#1.6 variable: Inter #hay un problema en 2011, pero
# se usa a partir del 2012

enaho_mod03_df6<-enaho_mod03_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,Inter)%>%
  summarise(
    tabla1=sum(factor03),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = Inter,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod03_df6[is.na(enaho_mod03_df6)]<-0

enaho_mod03_df6$total<-rowSums(enaho_mod03_df6[,3:length(enaho_mod03_df6)])

for (i in 3:(length(enaho_mod03_df6)-1)) {

  enaho_mod03_df6[,i]<-enaho_mod03_df6[,i]/enaho_mod03_df6[,length(enaho_mod03_df6)]
  colnames(enaho_mod03_df6)[i]<-str_c('inter','-',names(enaho_mod03_df6[,i]))
}

enaho_mod03_df6<-enaho_mod03_df6%>%
  select(-total)

#1.7 assessing mod03 data

cnt<- length(enaho_mod03_df6$ubigeo)+
      length(enaho_mod03_df2$ubigeo)+
      length(enaho_mod03_df3$ubigeo)+
      length(enaho_mod03_df4$ubigeo)+
      length(enaho_mod03_df5$ubigeo)+
      length(enaho_mod03_df6$ubigeo)

if(cnt==6*length(enaho_mod03_df6$ubigeo)){
  message('mod03 panel data is balanced')
}

#1.8. biding mod03 data

enaho_panel_mod03<-enaho_mod03_df1%>%
  left_join(enaho_mod03_df2,by=c('ubigeo','year'))%>%
  left_join(enaho_mod03_df3,by=c('ubigeo','year'))%>%
  left_join(enaho_mod03_df4,by=c('ubigeo','year'))%>%
  left_join(enaho_mod03_df5,by=c('ubigeo','year'))%>%
  left_join(enaho_mod03_df6,by=c('ubigeo','year'))

#1.9. saving mod03 panel data

#write_rds(enaho_panel_mod03,'enaho_panel_mod03.rds')

return(enaho_panel_mod03)

}
