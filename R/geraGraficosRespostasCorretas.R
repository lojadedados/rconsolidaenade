#' @title gera grafico medias
#' @description Funcao para gerar os graficos do ENADE
#' @author Vitor Almeida - vitoralm@gmail.com
#' @param per_comp: Array com resposta das categorias escolhidas
#' @param per_a: Respostas item A
#' @param per_b: Respostas item B
#' @param per_c: Respostas item C
#' @param per_d: Respostas item D
#' @param per_e: Respostas item E
#' @param per_n: Respostas Nao Preenchidas
#' @export
geraGraficosRespostasCorretas <- function(
  per_comp,
  per_a,
  per_b,
  per_c,
  per_d,
  per_e,
  per_n
) {

  #Graficos
  colors <- c()
  nomes_grafico <- c()
  dados_grafico <- c()
  titulo_grafico <- paste("Quest?o", q, "- Item", item_correto, sep=" ")
  colors <- c("yellow", "grey", "grey", "grey", "grey", "grey", "white", "white", "white", "white", "white")

  if (item_correto == "A") {
    dados_grafico = c(dados_grafico,per_comp[4],per_comp[1],per_comp[2],per_comp[3],per_comp[5],per_comp[6],per_b,per_c,per_d,per_e,per_n)
    nomes_grafico <- c("UNI7", "BR", "CE", "PRIVCE", "UFC", "UniC", "B","C","D","E","N")
  }
  else if (item_correto == "B") {
    dados_grafico = c(dados_grafico,per_comp[4],per_comp[1],per_comp[2],per_comp[3],per_comp[5],per_comp[6],per_a,per_c,per_d,per_e,per_n)
    nomes_grafico <- c("UNI7", "BR", "CE", "PRIVCE", "UFC", "UniC","A","C","D","E","N")
  }
  else if (item_correto == "C") {
    dados_grafico = c(dados_grafico,per_comp[4],per_comp[1],per_comp[2],per_comp[3],per_comp[5],per_comp[6],per_a,per_b,per_d,per_e,per_n)
    nomes_grafico <- c("UNI7", "BR", "CE", "PRIVCE", "UFC", "UniC","A","B","D","E","N")
  }
  else if (item_correto == "D") {
    dados_grafico = c(dados_grafico,per_comp[4],per_comp[1],per_comp[2],per_comp[3],per_comp[5],per_comp[6],per_a,per_b,per_c,per_e,per_n)
    nomes_grafico <- c("UNI7", "BR", "CE", "PRIVCE", "UFC", "UniC","A","B","C","E","N")
  }
  else if (item_correto == "E") {
    dados_grafico = c(dados_grafico,per_comp[4],per_comp[1],per_comp[2],per_comp[3],per_comp[5],per_comp[6],per_a,per_b,per_c,per_d,per_n)
    nomes_grafico <- c("UNI7", "BR", "CE", "PRIVCE", "UFC", "UniC","A","B","C","D","N")
  }

  png(file = paste("grafico_", q, ".png", sep=""))
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas",ylab = "Acertos",col = colors,
          ylim=c(0,1),main = titulo_grafico,border = "black")
  dev.off()
}
