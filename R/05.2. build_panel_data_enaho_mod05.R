#' creando data panel para modulo 5
#'
#' @param enaho_mod05_df df del modulo
#'
#' @return df en formato panel
#' @export
#'
#' @examples
#' panel_data_05(enaho_mod05_df)
panel_data_05=function(enaho_mod05_df){#0. packages
memory.limit (2000000)

if (!"sjlabelled" %in% installed.packages()[,"Package"]){install.packages("sjlabelled")}
library(sjlabelled)
library(tidyverse)
library(lubridate)

#1. PANEL DATA ENAHO MOD05
#enaho_mod05_df<-read_rds('enaho_clean_mod05.rds')

#names(enaho_mod05_df)

#VARIABLES A CONSIDERAR

# 1.1. "sex"       1.2. "status"       1.3. "y.old"**
# 1.4. "Rwork"     1.5. "Wwork"        1.6. "siz.frm"
# 1.7. 'otr.work'  1.8. 'pos.ocup'
# 1.9. 'juntos' 1.10. y.old

#1.1 variable: condici?n de viejo o joven #sirve

enaho_mod05_df1<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  mutate(y.old=case_when(
    y.old<26~'joven',
    y.old>=26~'viejo'
  ))%>%
  group_by(ubigeo,year,y.old)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = y.old,
              values_from = tabla1)%>%
  ungroup()#%>%
  #select(-'NA')

enaho_mod05_df1[is.na(enaho_mod05_df1)]<-0

enaho_mod05_df1$total<-rowSums(enaho_mod05_df1[,3:length(enaho_mod05_df1)])

for (i in 3:(length(enaho_mod05_df1)-1)) {

  enaho_mod05_df1[,i]<-enaho_mod05_df1[,i]/enaho_mod05_df1[,length(enaho_mod05_df1)]
  colnames(enaho_mod05_df1)[i]<-str_c(names(enaho_mod05_df1[,i]))
}

enaho_mod05_df1<-enaho_mod05_df1%>%
  select(-total)

#1.2 variable: sex #sirve

enaho_mod05_df2<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,sex)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = sex,
              values_from = tabla1)%>%
  ungroup()

enaho_mod05_df2[is.na(enaho_mod05_df2)]<-0

enaho_mod05_df2$total<-rowSums(enaho_mod05_df2[,3:length(enaho_mod05_df2)])

for (i in 3:(length(enaho_mod05_df2)-1)) {

  enaho_mod05_df2[,i]<-enaho_mod05_df2[,i]/enaho_mod05_df2[,length(enaho_mod05_df2)]
  colnames(enaho_mod05_df2)[i]<-str_c('sex','-',names(enaho_mod05_df2[,i]))
}

enaho_mod05_df2<-enaho_mod05_df2%>%
  select(-total)

#1.3 variable: status # no sirve

enaho_mod05_df3<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,status)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = status,
              values_from = tabla1)%>%
  ungroup()

enaho_mod05_df3[is.na(enaho_mod05_df3)]<-0

enaho_mod05_df3$total<-rowSums(enaho_mod05_df3[,3:length(enaho_mod05_df3)])

for (i in 3:(length(enaho_mod05_df3)-1)) {

  enaho_mod05_df3[,i]<-enaho_mod05_df3[,i]/enaho_mod05_df3[,length(enaho_mod05_df3)]
  colnames(enaho_mod05_df3)[i]<-str_c('status','-',names(enaho_mod05_df3[,i]))
}

enaho_mod05_df3<-enaho_mod05_df3%>%
  select(-total)

#1.4 variable: Rwork # no lo uso

enaho_mod05_df4<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,Rwork)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = Rwork,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod05_df4[is.na(enaho_mod05_df4)]<-0

enaho_mod05_df4$total<-rowSums(enaho_mod05_df4[,3:length(enaho_mod05_df4)])

for (i in 3:(length(enaho_mod05_df4)-1)) {

  enaho_mod05_df4[,i]<-enaho_mod05_df4[,i]/enaho_mod05_df4[,length(enaho_mod05_df4)]
  colnames(enaho_mod05_df4)[i]<-str_c('Rwork','-',names(enaho_mod05_df4[,i]))
}

enaho_mod05_df4<-enaho_mod05_df4%>%
  select(-total)

#1.5 variable: Wwork
# en ubigeos hay algunas opciones, y otras na
# varia entre los years. se procede a quitarlos y
# a no contarlos (0% por ejemplo,
# en la contabilidad del porcentaje)

enaho_mod05_df5<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,Wwork)%>%
  summarise(
    tabla1=sum(factor05),
  ) %>%
  ungroup()%>%
  filter(!is.na(Wwork))%>%
  pivot_wider(names_from = Wwork,
              values_from = tabla1)%>%
  ungroup()#%>%
  #select(-'NA')

enaho_mod05_df5[is.na(enaho_mod05_df5)]<-0

enaho_mod05_df5$total<-rowSums(enaho_mod05_df5[,3:length(enaho_mod05_df5)])

for (i in 3:(length(enaho_mod05_df5)-1)) {

  enaho_mod05_df5[,i]<-enaho_mod05_df5[,i]/enaho_mod05_df5[,length(enaho_mod05_df5)]
  colnames(enaho_mod05_df5)[i]<-str_c('Wwork','-',names(enaho_mod05_df5[,i]))
}

enaho_mod05_df5<-enaho_mod05_df5%>%
  select(-total)

#1.6 variable: siz.frm
# lo mismo que en 1.5
# hay problemas de ausencia de categorias en los years
# se procede a quitarse y a considerarlos nulos
# se cuenta la existencia de la categoria, sin valor reportado
# por lo que en porcentaje sera 0%

enaho_mod05_df6<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,siz.frm)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = siz.frm,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod05_df6[is.na(enaho_mod05_df6)]<-0

enaho_mod05_df6$total<-rowSums(enaho_mod05_df6[,3:length(enaho_mod05_df6)])

for (i in 3:(length(enaho_mod05_df6)-1)) {

  enaho_mod05_df6[,i]<-enaho_mod05_df6[,i]/enaho_mod05_df6[,length(enaho_mod05_df6)]
  colnames(enaho_mod05_df6)[i]<-str_c('siz.frm','-',names(enaho_mod05_df6[,i]))
}

enaho_mod05_df6<-enaho_mod05_df6%>%
  select(-total)

#1.7 variable: otr.work # no lo uso

enaho_mod05_df7<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,otr.work)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = otr.work,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod05_df7[is.na(enaho_mod05_df7)]<-0

enaho_mod05_df7$total<-rowSums(enaho_mod05_df7[,3:length(enaho_mod05_df7)])

for (i in 3:(length(enaho_mod05_df7)-1)) {

  enaho_mod05_df7[,i]<-enaho_mod05_df7[,i]/enaho_mod05_df7[,length(enaho_mod05_df7)]
  colnames(enaho_mod05_df7)[i]<-str_c('otr.work','-',names(enaho_mod05_df7[,i]))
}

enaho_mod05_df7<-enaho_mod05_df7%>%
  select(-total)

#1.8 variable: pos.ocup
# tiene los mismos problemas que vimos

enaho_mod05_df8<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,pos.ocup)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = pos.ocup,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod05_df8[is.na(enaho_mod05_df8)]<-0

enaho_mod05_df8$total<-rowSums(enaho_mod05_df8[,3:length(enaho_mod05_df8)])

for (i in 3:(length(enaho_mod05_df8)-1)) {

  enaho_mod05_df8[,i]<-enaho_mod05_df8[,i]/enaho_mod05_df8[,length(enaho_mod05_df8)]
  colnames(enaho_mod05_df8)[i]<-str_c('pos.ocup','-',names(enaho_mod05_df8[,i]))
}

enaho_mod05_df8<-enaho_mod05_df8%>%
  select(-total)

#1.8 variable: juntos #no lo uso
# tiene problemas

enaho_mod05_df9<-enaho_mod05_df%>%
  mutate(year=str_extract(year,'[0-9]+'))%>%
  group_by(ubigeo,year,juntos)%>%
  summarise(
    tabla1=sum(factor05),
  )%>%
  ungroup()%>%
  pivot_wider(names_from = juntos,
              values_from = tabla1)%>%
  ungroup()%>%
  select(-'NA')

enaho_mod05_df9[is.na(enaho_mod05_df9)]<-0

enaho_mod05_df9$total<-rowSums(enaho_mod05_df9[,3:length(enaho_mod05_df9)])

for (i in 3:(length(enaho_mod05_df9)-1)) {

  enaho_mod05_df9[,i]<-enaho_mod05_df9[,i]/enaho_mod05_df9[,length(enaho_mod05_df9)]
  colnames(enaho_mod05_df9)[i]<-str_c('juntos','-',names(enaho_mod05_df9[,i]))
}

enaho_mod05_df9<-enaho_mod05_df9%>%
  select(-total)

#1.9 variable: y.old

enaho_mod05_df10<-enaho_mod05_df%>%
  group_by(ubigeo,year)%>%
  summarise(
    y.oldmean=weighted.mean(y.old,factor05),
  )%>%
  ungroup()

#1.9 assessing mod05 data

cnt<- length(enaho_mod05_df6$ubigeo)+
  length(enaho_mod05_df2$ubigeo)+
  length(enaho_mod05_df3$ubigeo)+
  length(enaho_mod05_df4$ubigeo)+
  length(enaho_mod05_df5$ubigeo)+
  length(enaho_mod05_df6$ubigeo)+
  length(enaho_mod05_df7$ubigeo)+
  length(enaho_mod05_df8$ubigeo)+
  length(enaho_mod05_df9$ubigeo)#+
  #length(enaho_mod05_df10$ubigeo)

if(cnt==9*length(enaho_mod05_df6$ubigeo)){
  message('mod05 panel data is balanced')
}

#1.10. biding mod05 data

enaho_panel_mod05<-enaho_mod05_df1%>%
  left_join(enaho_mod05_df2,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df3,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df4,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df5,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df6,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df7,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df8,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df9,by=c('ubigeo','year'))%>%
  left_join(enaho_mod05_df10,by=c('ubigeo','year'))

#1.9. saving mod05 panel data

#write_rds(enaho_panel_mod05,'enaho_panel_mod05.rds')
return(enaho_panel_mod05)
}
