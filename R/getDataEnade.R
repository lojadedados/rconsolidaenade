#' @title recupera Dados Enade
#' @description Funcao para gerar os resultados do Enade
#' @author Vitor Almeida - vitoralm@gmail.com
#' @export
getDataEnade <- function() {
  dadosEnade <<- read.csv2("data/resultado.txt", sep = ";")
  dadosEnade
}
