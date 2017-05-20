#' @title gera grafico medias
#' @description Funcao para gerar os graficos do ENADE
#' @author Vitor Almeida - vitoralm@gmail.com
#' @param media_nt_fg_d1_pt_comp: Media de Nota de Lingua Portuguesa da questao 1 da parte discursiva da formacao geral
#' @param media_nt_fg_d1_ct_comp: Media de Nota de Conteudo da questao 1 da parte discursiva da formacao geral
#' @param media_nt_fg_d2_pt_comp: Media de Nota de Lingua Portuguesa da questao 2 da parte discursiva da formacao geral
#' @param media_nt_fg_d2_ct_comp: Media de Nota de Conteudo da questao 2 da parte discursiva da formacao geral
#' @param media_nt_ce_d1_comp: Media de Nota da questao 1 da parte discursiva do componente especifico - Convertida para escala de 0 a 100
#' @param media_nt_ce_d2_comp: Media de Nota da questao 2 da parte discursiva do componente especifico - Convertida para escala de 0 a 100
#' @param media_nt_ce_d3_comp: Media de Nota da questao 3 da parte discursiva do componente especifico - Convertida para escala de 0 a 100
#' @export
#'
geraGraficosMedias <- function(media_nt_fg_d1_pt_comp,
                       media_nt_fg_d1_ct_comp,
                       media_nt_fg_d2_pt_comp,
                       media_nt_fg_d2_ct_comp,
                       media_nt_ce_d1_comp,
                       media_nt_ce_d2_comp,
                       media_nt_ce_d3_comp
                       ) {

  #Gera os Gr?ficos
  nomes_grafico <- c("UNI7", "BR", "CE", "PrivCE", "UFC", "UniC")
  colors <- c("yellow", "grey", "grey", "grey", "grey", "grey")

  #grafico_nt_fg_d1_pt
  dados_grafico = c(media_nt_fg_d1_pt_comp[4], media_nt_fg_d1_pt_comp[1], media_nt_fg_d1_pt_comp[2], media_nt_fg_d1_pt_comp[3],
                    media_nt_fg_d1_pt_comp[5], media_nt_fg_d1_pt_comp[6])
  png(file = "grafico_nt_fg_d1_pt.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "FG1 - Portugu?s", border = "black")
  dev.off()

  #grafico_nt_fg_d1_ct
  dados_grafico = c(media_nt_fg_d1_ct_comp[4], media_nt_fg_d1_ct_comp[1], media_nt_fg_d1_ct_comp[2], media_nt_fg_d1_ct_comp[3],
                    media_nt_fg_d1_ct_comp[5], media_nt_fg_d1_ct_comp[6])
  png(file = "grafico_nt_fg_d1_ct.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "FG1 - Conte?do", border = "black")
  dev.off()

  #grafico_nt_fg_d2_pt
  dados_grafico = c(media_nt_fg_d2_pt_comp[4], media_nt_fg_d2_pt_comp[1], media_nt_fg_d2_pt_comp[2], media_nt_fg_d2_pt_comp[3],
                    media_nt_fg_d2_pt_comp[5], media_nt_fg_d2_pt_comp[6])
  png(file = "grafico_nt_fg_d2_pt.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "FG2 - Portugu?s", border = "black")
  dev.off()

  #grafico_nt_fg_d2_ct
  dados_grafico = c(media_nt_fg_d2_ct_comp[4], media_nt_fg_d2_ct_comp[1], media_nt_fg_d2_ct_comp[2], media_nt_fg_d2_ct_comp[3],
                    media_nt_fg_d2_ct_comp[5], media_nt_fg_d2_ct_comp[6])
  png(file = "grafico_nt_fg_d2_ct.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "FG2 - Conte?do", border = "black")
  dev.off()

  #grafico_nt_ce_d1
  dados_grafico = c(media_nt_ce_d1_comp[4], media_nt_ce_d1_comp[1], media_nt_ce_d1_comp[2], media_nt_ce_d1_comp[3],
                    media_nt_ce_d1_comp[5], media_nt_ce_d1_comp[6])
  png(file = "grafico_nt_ce_d1.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "Componente Espec?fico 1", border = "black")
  dev.off()

  #grafico_nt_ce_d2
  dados_grafico = c(media_nt_ce_d2_comp[4], media_nt_ce_d2_comp[1], media_nt_ce_d2_comp[2], media_nt_ce_d2_comp[3],
                    media_nt_ce_d2_comp[5], media_nt_ce_d2_comp[6])
  png(file = "grafico_nt_ce_d2.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "Componente Espec?fico 2", border = "black")
  dev.off()

  #grafico_nt_ce_d3
  dados_grafico = c(media_nt_ce_d3_comp[4], media_nt_ce_d3_comp[1], media_nt_ce_d3_comp[2], media_nt_ce_d3_comp[3],
                    media_nt_ce_d3_comp[5], media_nt_ce_d3_comp[6])
  png(file = "grafico_nt_ce_d3.png")
  barplot(dados_grafico, names.arg = nomes_grafico, xlab = "Respostas", ylab = "Acertos", col = colors,
          ylim=c(0,100), main = "Componente Espec?fico 3", border = "black")
  dev.off()

}
