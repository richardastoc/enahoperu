#' elige el sub-conjunto de datos de interes
#'
#' @param enaho_modrr modulo de analisis
#' @param global_index_modrr los indices de interes
#'
#' @return el df de cada modulo
#' @export
#'
#' @examples
#' select_subset(enaho_modrr,global_index_modrr)
select_subset<-function(enaho_modrr,global_index_modrr){

    enaho_raw_modrr<-list()

  for(t in 1:length(enaho_modrr)){

  if(isTRUE(global_index_modrr[[t]]$type[1]==1)){

    enaho_raw_modrr[[t]]<-enaho_modrr[[t]][[1]][,global_index_modrr[[t]]$index]

  }

  if(isTRUE(global_index_modrr[[t]]$type[1]==2)){

    enaho_raw_modrr[[t]]<-enaho_modrr[[t]][[2]][,global_index_modrr[[t]]$index]

  }


  }

    return(enaho_raw_modrr)
}


