#' @title gera dados
#' @description Funcao para gerar os graficos do ENADE
#' @author Vitor Almeida - vitoralm@gmail.com
#' @export
getData <- function() {

  if (is.null(data) || is.null(nrow(data))) {
    data <- subset(read.table("data/microdados_enade_2014_SI.csv",header=T,sep=";"), co_grupo == 4006)
  }

  data
}
