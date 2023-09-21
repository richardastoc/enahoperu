#' Limpieza de datos, corregir factores de expansion y ubigeo
#'
#' @param enaho_bind_mod03 df modulo 3
#' @param enaho_bind_mod05 df modulo 5
#' @param enaho_bind_mod85 df modulo 85
#'
#' @return un df para cada modulo limpiado
#' @export
#'
#' @examples
#' clean_data(enaho_bind_mod03,enaho_bind_mod05,enaho_bind_mod85)
clean_data=function(enaho_bind_mod03,
                    enaho_bind_mod05,
                    enaho_bind_mod85){
#0. packages
memory.limit (2000000)

if (!"sjlabelled" %in% installed.packages()[,"Package"]){install.packages("sjlabelled")}
library(sjlabelled)
library(tidyverse)
library(lubridate)

#1. load ubigeo_inei data

# DPTR<-readxl::read_xlsx("ubigeo_inei.xlsx",
#                     'UBIGEO') %>%
#   select(ubigeo,provincia)%>%
#   mutate(ubigeo=as.character(str_sub(ubigeo,1,4)),
#          provincia=as.character(str_to_lower(provincia)))%>%
#   unique()%>%
#   mutate(provincia=chartr("", "AEIOU", toupper(provincia)))
#

#1.1 recode the factor variable
#1.1.1 recode the factor mod03

enaho_temp_mod03<-enaho_bind_mod03%>%
  mutate(FACPOBTRIM=case_when(
         !is.na(FACPOBTRIM)~FACPOBTRIM,
         is.na(FACPOBTRIM)~0
         ),
         FACTOR=case_when(
           !is.na(FACTOR)~FACTOR,
           is.na(FACTOR)~0
         ),
         FACTORPOB=case_when(
           !is.na(FACTORPOB)~FACTORPOB,
           is.na(FACTORPOB)~0
         ),
         factor03=FACPOBTRIM+FACTOR+FACTORPOB
         )%>%
        filter(factor03!=0)%>%
        select(
          -FACPOBTRIM,
          -FACTOR,
          -FACTORPOB
        )

#1.1.2 recode the factor mod05

enaho_temp_mod05<-enaho_bind_mod05%>%
  mutate(FAC500TRIM=case_when(
          !is.na(FAC500TRIM)~FAC500TRIM,
          is.na(FAC500TRIM)~0
        ),
        FACTRIM500=case_when(
          !is.na(FACTRIM500)~FACTRIM500,
          is.na(FACTRIM500)~0
        ),
        FAC500=case_when(
          !is.na(FAC500)~FAC500,
          is.na(FAC500)~0
        ),
        FACTOR500=case_when(
          !is.na(FACTOR500)~FACTOR500,
          is.na(FACTOR500)~0
        ),
        FAC500A=case_when(
          !is.na(FAC500A)~FAC500A,
          is.na(FAC500A)~0
        ),
        factor05=FAC500TRIM+FACTRIM500+FAC500+FACTOR500+FAC500A
        )%>%
        filter(factor05!=0)%>%
        select(
          -FAC500TRIM,
          -FACTRIM500,
          -FAC500,
          -FACTOR500,
          -FAC500A,
          -DOMINIO,
          -ESTRATO
        )

#1.1.3 recode the factor mod85

enaho_temp_mod85<-enaho_bind_mod85%>%
        mutate(FACTOR=case_when(
          !is.na(FACTOR)~FACTOR,
          is.na(FACTOR)~0
        ),
        FACGOB=case_when(
          !is.na(FACGOB)~FACGOB,
          is.na(FACGOB)~0
        ),
        FACGOBTRIM=case_when(
          !is.na(FACGOBTRIM)~FACGOBTRIM,
          is.na(FACGOBTRIM)~0
        ),
        FAMIEGOBTRIM=case_when(
          !is.na(FAMIEGOBTRIM)~FAMIEGOBTRIM,
          is.na(FAMIEGOBTRIM)~0
        ),

        factor85=FACTOR+FACGOB+FACGOBTRIM+FAMIEGOBTRIM
        )%>%
        filter(factor85!=0)%>%
        select(
          -FACTOR,
          -FACGOB,
          -FACGOBTRIM,
          -FAMIEGOBTRIM
        )

#1.2 recode the variables mod03

enaho_temp_mod03$P301A<- factor(enaho_temp_mod03$P301A,labels = c(get_labels(enaho_temp_mod03$P301A)))
enaho_temp_mod03$P301D<- factor(enaho_temp_mod03$P301D,labels = c(get_labels(enaho_temp_mod03$P301D)))
enaho_temp_mod03$P308A<- factor(enaho_temp_mod03$P308A,labels = c(get_labels(enaho_temp_mod03$P308A)))
enaho_temp_mod03$P308D<- factor(enaho_temp_mod03$P308D,labels = c(get_labels(enaho_temp_mod03$P308D)))
enaho_temp_mod03$P310 <- factor(enaho_temp_mod03$P310, labels = c(get_labels(enaho_temp_mod03$P310)))
enaho_temp_mod03$P314A<- factor(enaho_temp_mod03$P314A,labels = c(get_labels(enaho_temp_mod03$P314A)))

#1.3 recode the variables mod05

enaho_temp_mod05$P207<-factor(enaho_temp_mod05$P207,labels=c(get_labels(enaho_temp_mod05$P207)))
enaho_temp_mod05$P209<-factor(enaho_temp_mod05$P209,labels=c(get_labels(enaho_temp_mod05$P209)))
enaho_temp_mod05$P301A<-factor(enaho_temp_mod05$P301A,labels=c(get_labels(enaho_temp_mod05$P301A)))
enaho_temp_mod05$P501<-factor(enaho_temp_mod05$P501,labels=c(get_labels(enaho_temp_mod05$P501)))
enaho_temp_mod05$P510<-factor(enaho_temp_mod05$P510,labels=c('NA',get_labels(enaho_temp_mod05$P510)))
enaho_temp_mod05$P514<-factor(enaho_temp_mod05$P514,labels=c(get_labels(enaho_temp_mod05$P514)))
enaho_temp_mod05$P512A<-factor(enaho_temp_mod05$P512A,labels=c(get_labels(enaho_temp_mod05$P512A)))
enaho_temp_mod05$P517<-factor(enaho_temp_mod05$P517,labels=c(get_labels(enaho_temp_mod05$P517)))
enaho_temp_mod05$P5293A<-factor(enaho_temp_mod05$P5293A,labels=c('NA',get_labels(enaho_temp_mod05$P5293A)))
enaho_temp_mod05$P5294A<-factor(enaho_temp_mod05$P5294A,labels=c('NA',get_labels(enaho_temp_mod05$P5294A)))
enaho_temp_mod05$P5566A<-factor(enaho_temp_mod05$P5566A,labels=c(get_labels(enaho_temp_mod05$P5566A)))

#1.4 recode the variables mod85

enaho_temp_mod85$'P1$04'  <- factor(enaho_temp_mod85$'P1$04',labels = c(get_labels(enaho_temp_mod85$'P1$04')))
enaho_temp_mod85$'P2_1$01'<- factor(enaho_temp_mod85$'P2_1$01',labels = c(get_labels(enaho_temp_mod85$'P2_1$01')))
enaho_temp_mod85$'P2_1$02'<- factor(enaho_temp_mod85$'P2_1$02',labels = c(get_labels(enaho_temp_mod85$'P2_1$02')))
enaho_temp_mod85$'P2_1$03'<- factor(enaho_temp_mod85$'P2_1$03',labels = c(get_labels(enaho_temp_mod85$'P2_1$03')))
enaho_temp_mod85$'P2_1$08'<- factor(enaho_temp_mod85$'P2_1$08',labels = c(get_labels(enaho_temp_mod85$'P2_1$08')))

#1.5. recode month by quarter
#1.5.1. recode date=year mod03

enaho_mod03_df<-enaho_temp_mod03%>%
  mutate(quarter=str_extract(period_mod,'Trimestre [0-9]'))%>%
  mutate(year=case_when(
                        quarter=='Trimestre 1'~str_c(ayo,'Q1'),
                        quarter=='Trimestre 2'~str_c(ayo,'Q2'),
                        quarter=='Trimestre 3'~str_c(ayo,'Q3'),
                        quarter=='Trimestre 4'~str_c(ayo,'Q4'),
                        TRUE~'Incoherencia'
                        ),
         ubigeo=str_sub(UBIGEO,1,4)
       )%>%
  # left_join(
  #   DPTR%>%
  #     select(ubigeo,provincia),by='ubigeo'
  # )%>%
  # filter(str_sub(ubigeo,1,2)=='05')%>%
  select(
    -ayo,
    -MES,
    -quarter,
    -UBIGEO
  )%>%
  rename(
   N.REdu=P301A,
   C.REst=P301D,
   N.CEdu=P308A,
   C.CEst=P308D,
   Capac=P310,
   Inter=P314A
  )

#1.5.2. recode date=year mod05

enaho_mod05_df<-enaho_temp_mod05%>%
  mutate(quarter=str_extract(period_mod,'Trimestre [0-9]'))%>%
  mutate(year=case_when(
    quarter=='Trimestre 1'~str_c(ayo,'Q1'),
    quarter=='Trimestre 2'~str_c(ayo,'Q2'),
    quarter=='Trimestre 3'~str_c(ayo,'Q3'),
    quarter=='Trimestre 4'~str_c(ayo,'Q4'),
    TRUE~'Incoherencia'
  ),
  ubigeo=str_sub(UBIGEO,1,4)
  )%>%
  # left_join(
  #   DPTR%>%
  #     select(ubigeo,provincia),by='ubigeo'
  # )%>%
  # filter(str_sub(ubigeo,1,2)=='05')%>%
  select(
    -ayo,
    -MES,
    -quarter,
    -UBIGEO
  )%>%
  rename(
    sex=P207,
    y.old=P208A,
    status=P209,
    N.REdu=P301A,
    Rwork=P501,
    Wwork=P510,
    siz.frm=P512A,
    otr.work=P514,
    pos.ocup=P517,
    fpay.transp=P5293A,
    fpay.vivi=P5294A,
    juntos=P5566A,
  )

#1.5.3. recode date=year mod85

enaho_mod85_df<-enaho_temp_mod85%>%
  mutate(quarter=str_extract(period_mod,'Trimestre [0-9]'))%>%
  mutate(year=case_when(
    quarter=='Trimestre 1'~str_c(ayo,'Q1'),
    quarter=='Trimestre 2'~str_c(ayo,'Q2'),
    quarter=='Trimestre 3'~str_c(ayo,'Q3'),
    quarter=='Trimestre 4'~str_c(ayo,'Q4'),
    TRUE~'Incoherencia'
  ),
  ubigeo=str_sub(UBIGEO,1,4)
  )%>%
  # left_join(
  #   DPTR%>%
  #     select(ubigeo,provincia),by='ubigeo'
  # )%>%
  # filter(str_sub(ubigeo,1,2)=='05')%>%
  select(
    -ayo,
    -MES,
    -quarter,
    -UBIGEO
  )%>%
  rename(
    conf_muni='P1$04',
    main.corrp='P2_1$01',
    main.gob.credi='P2_1$02',
    main.mply='P2_1$03',
    edu.priority='P2_1$08'
  )

#1.6.summary
#1.6.1.this sum all factors by quarter

#write_rds(enaho_mod03_df%>%select(-ubigeo),'enaho_clean_mod03.rds')
#write_rds(enaho_mod05_df%>%select(-ubigeo),'enaho_clean_mod05.rds')
#write_rds(enaho_mod85_df%>%select(-ubigeo),'enaho_clean_mod85.rds')

return(list(enaho_mod03_df=enaho_mod03_df,
            enaho_mod05_df=enaho_mod05_df,
            enaho_mod85_df=enaho_mod85_df))

}
