#' es la 2da pre-funcion
#'
#' @param doc el documento o modulo a analizar
#' @param var la variable a extraer
#'
#' @return un df para cada modulo de cada variable
#' @export
#'
#' @examples
#' select_goals(doc,var)
select_goals<-function(doc,var){

#doc=enaho_mod85
#var=variables_mod85

col_index_modxx<-list()
col_names_modxx<-list()
col_facto_modxx<-list()

for (y in 1:length(doc)) {

  index1_modxx<-list()
  index2_modxx<-list()
  #y=38

  for (n in 1:nrow(var)) {

    #1.2. index list, index factor variable and names, to interest variables in every year
    #1.2.1. if there is one type
    #n=1

    if(length(doc[[y]])<=1){

      index1_modxx[[n]]<-tibble(index=
                                  which(names(doc[[y]][[1]])==var$variable[n]),
                                name=var$variable[n],
                                year=doc[[y]][[1]]$period_mod[1],
                                type=1
      )
      index2_modxx[[n]]<-tibble(index=NA,
                                name=NA,
                                year=NA,
                                type=NA
      )

      index1_factxx<-names(doc[[y]][[1]])[which(str_to_lower(names(doc[[y]][[1]]))%>%
                                                          str_detect('fa+'))]
      index2_factxx<-NA

      col_facto_1<-tibble(factor=index1_factxx,
                          index=which(str_to_lower(names(doc[[y]][[1]]))%>%
                                        str_detect('fa+')),
                          type=1)
      col_facto_2<-tibble(factor=index2_factxx,
                          index=NA,
                          type=NA)

      indexn1_modxx<-tibble(names=names(doc[[y]][[1]]),
                            year=doc[[y]][[1]]$period_mod[1],
                            type=1
      )
      indexn2_modxx<-tibble(names=NA,
                            year=NA,
                            type=NA
      )

    #1.2.2. if there is two or more type
    }else{

      index1_modxx[[n]]<-tibble(index=
                                  which(names(doc[[y]][[1]])==var$variable[n]),
                                name=var$variable[n],
                                year=doc[[y]][[1]]$period_mod[1],
                                type=1
      )
      index2_modxx[[n]]<-tibble(index=
                                  which(names(doc[[y]][[2]])==var$variable[n]),
                                name=var$variable[n],
                                year=doc[[y]][[2]]$period_mod[1],
                                type=2
      )

      index1_factxx<-names(doc[[y]][[1]])[which(str_to_lower(names(doc[[y]][[1]]))%>%
                                                          str_detect('fa+'))]
      index2_factxx<-names(doc[[y]][[2]])[which(str_to_lower(names(doc[[y]][[2]]))%>%
                                                          str_detect('fa+'))]
      col_facto_1<-tibble(factor=index1_factxx,
                          index=which(str_to_lower(names(doc[[y]][[1]]))%>%
                                        str_detect('fa+')),
                          type=1)
      col_facto_2<-tibble(factor=index2_factxx,
                          index=which(str_to_lower(names(doc[[y]][[2]]))%>%
                                        str_detect('fa+')),
                          type=2)

      indexn1_modxx<-tibble(names=names(doc[[y]][[1]]),
                            year=doc[[y]][[1]]$period_mod[1],
                            type=1
      )
      indexn2_modxx<-tibble(names=names(doc[[y]][[2]]),
                            year=doc[[y]][[2]]$period_mod[1],
                            type=2
      )
    }
  }

  #1.2.3. bind index{i}_modxx

  index11_modxx<-bind_rows(index1_modxx)
  index22_modxx<-na.omit(bind_rows(index2_modxx))

  #1.3. saving index to every year

  if(isTRUE(length(index11_modxx$index)!=0)){
    col_index_modxx[[y]]<-index11_modxx
    col_names_modxx[[y]]<-indexn1_modxx
  }

  #1.3.n warning message appears when index22_modxx is empty

  if(isTRUE(length(index22_modxx$index)!=0)){
    col_index_modxx[[y]]<-index22_modxx
    col_names_modxx[[y]]<-indexn2_modxx
  }
  if(isTRUE(length(index11_modxx$index)==0) & isTRUE(length(index22_modxx$index)==0)){
    col_index_modxx[[y]]<-doc[[y]][[1]]$period_mod[1]
    col_names_modxx[[y]]<-bind_cols(indexn1_modxx,indexn2_modxx)
  }


  col_facto_modxx[[y]]<-na.omit(bind_rows(col_facto_1,col_facto_2))

}

return(c(col_index_modxx,
       col_names_modxx,
       col_facto_modxx)
       )

}

