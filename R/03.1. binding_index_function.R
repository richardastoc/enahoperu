#' Es una funcion-input para elegir los indices de las variables
#'
#' @param general_index_modyy indice general de la descarga
#' @param col_facto_modyy indice del factor de expansion
#' @param col_index_modyy indice del modulo
#'
#' @return un df de indices
#' @export
#'
#' @examples
#' binding_index(general_index_modyy,col_facto_modyy,col_index_modyy)
binding_index<-function(general_index_modyy,col_facto_modyy,col_index_modyy){

global_index_modyy<-list()

for (y in 1:length(col_index_modyy)) {

  #y=30

  if(isTRUE(col_index_modyy[[y]]$type[1]==2)){

    ####

    factor_df<-col_facto_modyy[[y]][col_facto_modyy[[y]]$type=='2',]%>%
      mutate(
        year=col_index_modyy[[y]]$year[[1]],
        type=2
      )%>%
      rename(name=factor)

    global_index_modyy[[y]]<-general_index_modyy[[y]][[2]]%>%
      mutate(
        year=col_index_modyy[[y]]$year[[1]],
        type=2
      )%>%
      bind_rows(col_index_modyy[[y]]%>%
                  select(index,name,year,type))%>%
      bind_rows(factor_df)%>%
      distinct()

  }

  if(isTRUE(col_index_modyy[[y]]$type[1]==1)){

    if(is.null(dim(general_index_modyy[[y]]))){

      factor_df<-col_facto_modyy[[y]][col_facto_modyy[[y]]$type=='1',]%>%
        mutate(
          year=col_index_modyy[[y]]$year[[1]],
          type=1
        )%>%
        rename(name=factor)

      global_index_modyy[[y]]<-general_index_modyy[[y]][[1]]%>%
        mutate(
          year=col_index_modyy[[y]]$year[[1]],
          type=1
        )%>%
        bind_rows(col_index_modyy[[y]]%>%
                    select(index,name,year,type))%>%
        bind_rows(factor_df)%>%
        distinct()

    }else{

      factor_df<-col_facto_modyy[[y]][col_facto_modyy[[y]]$type=='1',]%>%
        mutate(
          year=col_index_modyy[[y]]$year[[1]],
          type=1
        )%>%
        rename(name=factor)

      global_index_modyy[[y]]<-general_index_modyy[[y]]%>%
        mutate(
          year=col_index_modyy[[y]]$year[[1]],
          type=1
        )%>%
        bind_rows(col_index_modyy[[y]]%>%
                    select(index,name,year,type))%>%
        bind_rows(factor_df)%>%
        distinct()
    }



  }

}

return(global_index_modyy)

}

