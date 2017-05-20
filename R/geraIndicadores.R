#' @title gera grafico medias
#' @description Funcao para gerar os graficos do ENADE
#' @author Vitor Almeida - vitoralm@gmail.com
#' @param geraGraficosMedias: Boleano para gerar Graficos Medias
#' @param geraGraficosRespostasCorretas: Boleano para gerar graficos Respostas Corretas
#' @import stringi
#' @export
geraIndicadores <- function(geraGraficosMedias = FALSE, geraGraficosRespostasCorretas = FALSE) {


  data <- getData()
  co_ies <- c(1556, 583, 1895)        #UNI7, UFC, UniChristus
  co_uf_curso <- 23     #UF CE
  co_catad <- 10005     #Instituição privada com fins lucrativos
  col_co_ies <- 3       #Coluna da Instituicao
  col_co_uf_curso <- 7  #Coluna da UF
  col_co_catad <- 4     #Coluna Categoria administrativa

  n <- nrow(data)       #total de alunos
  n_uf <- sum(data$co_uf_curso == co_uf_curso)  #total de alunos da UF
  n_catad <- length(which(data$co_catad == co_catad & data$co_uf_curso == co_uf_curso))  #total de alunos da categoria administrativa (privada)
  n_ies <- sum(data$co_ies == co_ies[1])  #total de alunos da ies
  n_comp <- c(n, n_uf, n_catad)

  #Valores para BR, UF, Catad
  soma_nt_fg_d1_pt_comp <- c(0, 0, 0)
  soma_nt_fg_d1_ct_comp <- c(0, 0, 0)
  soma_nt_fg_d2_pt_comp <- c(0, 0, 0)
  soma_nt_fg_d2_ct_comp <- c(0, 0, 0)
  soma_nt_ce_d1_comp <- c(0, 0, 0)
  soma_nt_ce_d2_comp <- c(0, 0, 0)
  soma_nt_ce_d3_comp <- c(0, 0, 0)
  media_nt_fg_d1_pt_comp <- c(0, 0, 0)
  media_nt_fg_d1_ct_comp <- c(0, 0, 0)
  media_nt_fg_d2_pt_comp <- c(0, 0, 0)
  media_nt_fg_d2_ct_comp <- c(0, 0, 0)
  media_nt_ce_d1_comp <- c(0, 0, 0, 0)
  media_nt_ce_d2_comp <- c(0, 0, 0)
  media_nt_ce_d3_comp <- c(0, 0, 0)



  #Valores para demais IES
  for (i in 1:length(co_ies)) {
    n_comp <- c(n_comp, sum(data$co_ies == co_ies[i]))
    soma_nt_fg_d1_pt_comp <- c(soma_nt_fg_d1_pt_comp, 0)
    soma_nt_fg_d1_ct_comp <- c(soma_nt_fg_d1_ct_comp, 0)
    soma_nt_fg_d2_pt_comp <- c(soma_nt_fg_d2_pt_comp, 0)
    soma_nt_fg_d2_ct_comp <- c(soma_nt_fg_d2_ct_comp, 0)
    soma_nt_ce_d1_comp <- c(soma_nt_ce_d1_comp, 0)
    soma_nt_ce_d2_comp <- c(soma_nt_ce_d2_comp, 0)
    soma_nt_ce_d3_comp <- c(soma_nt_ce_d3_comp, 0)
    media_nt_fg_d1_pt_comp <- c(media_nt_fg_d1_pt_comp, 0)
    media_nt_fg_d1_ct_comp <- c(media_nt_fg_d1_ct_comp, 0)
    media_nt_fg_d2_pt_comp <- c(media_nt_fg_d2_pt_comp, 0)
    media_nt_fg_d2_ct_comp <- c(media_nt_fg_d2_ct_comp, 0)
    media_nt_ce_d1_comp <- c(media_nt_ce_d1_comp, 0)
    media_nt_ce_d2_comp <- c(media_nt_ce_d2_comp, 0)
    media_nt_ce_d3_comp <- c(media_nt_ce_d3_comp, 0)
  }





  total_comp <- length(n_comp)
  print(n_comp)

  ##### PARTE DISCURSIVA #####
  col_nt_fg_d1_pt <- 52     #Coluna com Nota de L?ngua Portuguesa da quest?o 1 da parte discursiva da forma??o geral
  col_nt_fg_d1_ct <- 53     #Coluna com Nota de Conte?do da quest?o 1 da parte discursiva da forma??o geral
  col_nt_fg_d2_pt <- 55     #Coluna com Nota de L?ngua Portuguesa da quest?o 2 da parte discursiva da forma??o geral
  col_nt_fg_d2_ct <- 56     #Coluna com Nota de Conte?do da quest?o 2 da parte discursiva da forma??o geral
  col_nt_ce_d1 <- 61        #Coluna com Nota da quest?o 1 da parte discursiva do componente espec?fico - Convertida para escala de 0 a 100
  col_nt_ce_d2 <- 62        #Coluna com Nota da quest?o 2 da parte discursiva do componente espec?fico - Convertida para escala de 0 a 100
  col_nt_ce_d3 <- 63        #Coluna com Nota da quest?o 3 da parte discursiva do componente espec?fico - Convertida para escala de 0 a 100

  medias_comp <- list()

  #Itera sobre alunos
  for(a in 1:n) {
    ies <- data[a,col_co_ies]
    uf <- data[a,col_co_uf_curso]
    catad <- data[a,col_co_catad]

    nt_fg_d1_pt <- data[a,col_nt_fg_d1_pt]
    nt_fg_d1_ct <- data[a,col_nt_fg_d1_ct]
    nt_fg_d2_pt <- data[a,col_nt_fg_d2_pt]
    nt_fg_d2_ct <- data[a,col_nt_fg_d2_ct]
    nt_ce_d1 <- data[a,col_nt_ce_d1]
    nt_ce_d2 <- data[a,col_nt_ce_d2]
    nt_ce_d3 <- data[a,col_nt_ce_d3]

    soma_nt_fg_d1_pt_comp[1] <- if (nt_fg_d1_pt == "" || is.na(nt_fg_d1_pt)) soma_nt_fg_d1_pt_comp[1] else soma_nt_fg_d1_pt_comp[1] + nt_fg_d1_pt
    soma_nt_fg_d1_ct_comp[1] <- if (nt_fg_d1_ct == "" || is.na(nt_fg_d1_ct)) soma_nt_fg_d1_ct_comp[1] else soma_nt_fg_d1_ct_comp[1] + nt_fg_d1_ct
    soma_nt_fg_d2_pt_comp[1] <- if (nt_fg_d2_pt == "" || is.na(nt_fg_d2_pt)) soma_nt_fg_d2_pt_comp[1] else soma_nt_fg_d2_pt_comp[1] + nt_fg_d2_pt
    soma_nt_fg_d2_ct_comp[1] <- if (nt_fg_d2_ct == "" || is.na(nt_fg_d2_ct)) soma_nt_fg_d2_ct_comp[1] else soma_nt_fg_d2_ct_comp[1] + nt_fg_d2_ct
    soma_nt_ce_d1_comp[1] <- if (nt_ce_d1 == "" || is.na(nt_ce_d1)) soma_nt_ce_d1_comp[1] else soma_nt_ce_d1_comp[1] + nt_ce_d1
    soma_nt_ce_d2_comp[1] <- if (nt_ce_d2 == "" || is.na(nt_ce_d2)) soma_nt_ce_d2_comp[1] else soma_nt_ce_d2_comp[1] + nt_ce_d2
    soma_nt_ce_d3_comp[1] <- if (nt_ce_d3 == "" || is.na(nt_ce_d3)) soma_nt_ce_d3_comp[1] else soma_nt_ce_d3_comp[1] + nt_ce_d3

    if (uf == co_uf_curso) {
      soma_nt_fg_d1_pt_comp[2] <- if (nt_fg_d1_pt == "" || is.na(nt_fg_d1_pt)) soma_nt_fg_d1_pt_comp[2] else soma_nt_fg_d1_pt_comp[2] + nt_fg_d1_pt
      soma_nt_fg_d1_ct_comp[2] <- if (nt_fg_d1_ct == "" || is.na(nt_fg_d1_ct)) soma_nt_fg_d1_ct_comp[2] else soma_nt_fg_d1_ct_comp[2] + nt_fg_d1_ct
      soma_nt_fg_d2_pt_comp[2] <- if (nt_fg_d2_pt == "" || is.na(nt_fg_d2_pt)) soma_nt_fg_d2_pt_comp[2] else soma_nt_fg_d2_pt_comp[2] + nt_fg_d2_pt
      soma_nt_fg_d2_ct_comp[2] <- if (nt_fg_d2_ct == "" || is.na(nt_fg_d2_ct)) soma_nt_fg_d2_ct_comp[2] else soma_nt_fg_d2_ct_comp[2] + nt_fg_d2_ct
      soma_nt_ce_d1_comp[2] <- if (nt_ce_d1 == "" || is.na(nt_ce_d1)) soma_nt_ce_d1_comp[2] else soma_nt_ce_d1_comp[2] + nt_ce_d1
      soma_nt_ce_d2_comp[2] <- if (nt_ce_d2 == "" || is.na(nt_ce_d2)) soma_nt_ce_d2_comp[2] else soma_nt_ce_d2_comp[2] + nt_ce_d2
      soma_nt_ce_d3_comp[2] <- if (nt_ce_d3 == "" || is.na(nt_ce_d3)) soma_nt_ce_d3_comp[2] else soma_nt_ce_d3_comp[2] + nt_ce_d3
    }

    if (catad == co_catad && uf == co_uf_curso) {
      soma_nt_fg_d1_pt_comp[3] <- if (nt_fg_d1_pt == "" || is.na(nt_fg_d1_pt)) soma_nt_fg_d1_pt_comp[3] else soma_nt_fg_d1_pt_comp[3] + nt_fg_d1_pt
      soma_nt_fg_d1_ct_comp[3] <- if (nt_fg_d1_ct == "" || is.na(nt_fg_d1_ct)) soma_nt_fg_d1_ct_comp[3] else soma_nt_fg_d1_ct_comp[3] + nt_fg_d1_ct
      soma_nt_fg_d2_pt_comp[3] <- if (nt_fg_d2_pt == "" || is.na(nt_fg_d2_pt)) soma_nt_fg_d2_pt_comp[3] else soma_nt_fg_d2_pt_comp[3] + nt_fg_d2_pt
      soma_nt_fg_d2_ct_comp[3] <- if (nt_fg_d2_ct == "" || is.na(nt_fg_d2_ct)) soma_nt_fg_d2_ct_comp[3] else soma_nt_fg_d2_ct_comp[3] + nt_fg_d2_ct
      soma_nt_ce_d1_comp[3] <- if (nt_ce_d1 == "" || is.na(nt_ce_d1)) soma_nt_ce_d1_comp[3] else soma_nt_ce_d1_comp[3] + nt_ce_d1
      soma_nt_ce_d2_comp[3] <- if (nt_ce_d2 == "" || is.na(nt_ce_d2)) soma_nt_ce_d2_comp[3] else soma_nt_ce_d2_comp[3] + nt_ce_d2
      soma_nt_ce_d3_comp[3] <- if (nt_ce_d3 == "" || is.na(nt_ce_d3)) soma_nt_ce_d3_comp[3] else soma_nt_ce_d3_comp[3] + nt_ce_d3
    }

    for (i in 1:length(co_ies)) {
      if (ies == co_ies[i]) {
        soma_nt_fg_d1_pt_comp[3+i] <- if (nt_fg_d1_pt == "" || is.na(nt_fg_d1_pt)) soma_nt_fg_d1_pt_comp[3+i] else soma_nt_fg_d1_pt_comp[3+i] + nt_fg_d1_pt
        soma_nt_fg_d1_ct_comp[3+i] <- if (nt_fg_d1_ct == "" || is.na(nt_fg_d1_ct)) soma_nt_fg_d1_ct_comp[3+i] else soma_nt_fg_d1_ct_comp[3+i] + nt_fg_d1_ct
        soma_nt_fg_d2_pt_comp[3+i] <- if (nt_fg_d2_pt == "" || is.na(nt_fg_d2_pt)) soma_nt_fg_d2_pt_comp[3+i] else soma_nt_fg_d2_pt_comp[3+i] + nt_fg_d2_pt
        soma_nt_fg_d2_ct_comp[3+i] <- if (nt_fg_d2_ct == "" || is.na(nt_fg_d2_ct)) soma_nt_fg_d2_ct_comp[3+i] else soma_nt_fg_d2_ct_comp[3+i] + nt_fg_d2_ct
        soma_nt_ce_d1_comp[3+i] <- if (nt_ce_d1 == "" || is.na(nt_ce_d1)) soma_nt_ce_d1_comp[3+i] else soma_nt_ce_d1_comp[3+i] + nt_ce_d1
        soma_nt_ce_d2_comp[3+i] <- if (nt_ce_d2 == "" || is.na(nt_ce_d2)) soma_nt_ce_d2_comp[3+i] else soma_nt_ce_d2_comp[3+i] + nt_ce_d2
        soma_nt_ce_d3_comp[3+i] <- if (nt_ce_d3 == "" || is.na(nt_ce_d3)) soma_nt_ce_d3_comp[3+i] else soma_nt_ce_d3_comp[3+i] + nt_ce_d3
      }
    }
  }

  #Calcula as m?dias
  for(i in 1:total_comp)
  {

    media_nt_fg_d1_pt_comp[i] <- soma_nt_fg_d1_pt_comp[i]/n_comp[i]
    media_nt_fg_d1_ct_comp[i] <- soma_nt_fg_d1_ct_comp[i]/n_comp[i]
    media_nt_fg_d2_pt_comp[i] <- soma_nt_fg_d2_pt_comp[i]/n_comp[i]
    media_nt_fg_d2_ct_comp[i] <- soma_nt_fg_d2_ct_comp[i]/n_comp[i]
    media_nt_ce_d1_comp[i] <- soma_nt_ce_d1_comp[i]/n_comp[i]
    media_nt_ce_d2_comp[i] <- soma_nt_ce_d2_comp[i]/n_comp[i]
    media_nt_ce_d3_comp[i] <- soma_nt_ce_d3_comp[i]/n_comp[i]
    medias_comp[[length(medias_comp)+1]] <- list(c(media_nt_fg_d1_pt_comp[i], media_nt_fg_d1_ct_comp[i], media_nt_fg_d2_pt_comp[i],
                                                   media_nt_fg_d2_ct_comp[i], media_nt_ce_d1_comp[i], media_nt_ce_d2_comp[i],
                                                   media_nt_ce_d3_comp[i]))
  }

  # Gera arquivo texto
  nomes_provas <- c("FGPt1","FGCt1","FGPt2","FGCt2","CE1","CE2","CE3")
  res.data <- data.frame(
    Provas = nomes_provas,
    UNI7 = medias_comp[4],
    Brasil = medias_comp[1],
    CE = medias_comp[2],
    PrivadasCE = medias_comp[3],
    IES1 = medias_comp[5],
    IES2 = medias_comp[6]
  )

  write.table(res.data,"data\\resultadoDisc.txt",sep="\t",row.names=FALSE)

  ### GERA GRAFICOS DE MEDIAS ###
  if (geraGraficosMedias) {
    geraGraficosMedias()
  }

  ###############################

  ##### PARTE OBJETIVA #####
  col_vt_gab_ofg_fin <- 29  #Coluna com gabarito da forma??o geral
  col_vt_gab_oce_fin <- 35  #Coluna com gabarito de componentes espec?ficos
  col_vt_esc_ofg <- 47      #Coluna com escolhas da forma??o geral
  col_vt_esc_oce <- 49      #Coluna com escolhas de componentes espec?ficos

  gab <- paste(data[1,col_vt_gab_ofg_fin],data[1,col_vt_gab_oce_fin],sep="") #jun??o dos gabaritos
  n_questoes <- stri_length(gab)  #total de quest?es

  #Vetores de respostas das quest?es objetivas
  resposta_item_a <- c()
  resposta_item_b <- c()
  resposta_item_c <- c()
  resposta_item_d <- c()
  resposta_item_e <- c()
  resposta_item_n <- c()
  num_questoes <- c()

  resposta_correta_comp <- list()
  for (i in 1:total_comp){
    resposta_correta_comp[[i]] <- numeric()
  }


  for (q in 1:n_questoes) {
    msg <- paste("Interando ", q, " de ", n_questoes, sep = "")
    print(msg)
    item_correto <- substr(gab, q, q)
    if (!(item_correto %in% c("A","B","C","D","E"))) next

    n_item_a <- 0
    n_item_b <- 0
    n_item_c <- 0
    n_item_d <- 0
    n_item_e <- 0
    n_item_n <- 0
    n_acertos_comp <- c(0, 0, 0)
    for (i in 1:length(co_ies)){
      n_acertos_comp <- c(n_acertos_comp, 0)
    }

    #Itera sobre alunos
    for(a in 1:n) {
      escolhas <- paste(data[a,col_vt_esc_ofg],data[a,col_vt_esc_oce],sep="")
      item_marcado <- substr(escolhas, q, q)
      ies <- data[a,col_co_ies]
      uf <- data[a,col_co_uf_curso]
      catad <- data[a,col_co_catad]

      if (item_marcado == item_correto) {
        n_acertos_comp[1] <- n_acertos_comp[1] + 1
        if (uf == co_uf_curso) { n_acertos_comp[2] <- n_acertos_comp[2] + 1 }
        if (catad == co_catad && uf == co_uf_curso) { n_acertos_comp[3] <- n_acertos_comp[3] + 1 }
        for (i in 1:length(co_ies)) {
          if (ies == co_ies[i]) { n_acertos_comp[3+i] <- n_acertos_comp[3+i] + 1 }
        }
      }

      #Contabiliza respostas da UNI7
      if (ies == co_ies[1]) {
        if (item_marcado == "A") { n_item_a <- n_item_a + 1 }
        else if (item_marcado == "B") { n_item_b <- n_item_b + 1 }
        else if (item_marcado == "C") { n_item_c <- n_item_c + 1 }
        else if (item_marcado == "D") { n_item_d <- n_item_d + 1 }
        else if (item_marcado == "E") { n_item_e <- n_item_e + 1 }
        else { n_item_n <- n_item_n + 1 }
      }
    }

    #Percentuais
    num_questoes <- c(num_questoes,q)
    per_a <- n_item_a/n_ies
    per_b <- n_item_b/n_ies
    per_c <- n_item_c/n_ies
    per_d <- n_item_d/n_ies
    per_e <- n_item_e/n_ies
    per_n <- n_item_n/n_ies
    resposta_item_a <- c(resposta_item_a,format(per_a,2))
    resposta_item_b <- c(resposta_item_b,format(per_b,2))
    resposta_item_c <- c(resposta_item_c,format(per_c,2))
    resposta_item_d <- c(resposta_item_d,format(per_d,2))
    resposta_item_e <- c(resposta_item_e,format(per_e,2))
    resposta_item_n <- c(resposta_item_n,format(per_n,2))

    per_comp <- c()

    for(i in 1:total_comp) {
      per_comp <- c(per_comp, n_acertos_comp[i]/n_comp[i])
      resposta_correta_comp[[i]] <- c(resposta_correta_comp[[i]],format(per_comp[i],2))
    }

    ###############################
    if (geraGraficosRespostasCorretas) {
      geraGraficosRespostasCorretas()
    }
    ###############################

  }





  res.data <- data.frame(
    Questoes = num_questoes,
    UNI7 = resposta_correta_comp[4],
    Brasil = resposta_correta_comp[1],
    CE = resposta_correta_comp[2],
    PrivadasCE = resposta_correta_comp[3],
    A = resposta_item_a,
    B = resposta_item_b,
    C = resposta_item_c,
    D = resposta_item_d,
    E = resposta_item_e,
    N = resposta_item_n
  )

  write.table(res.data,"data\\resultadoObj.txt",sep="\t",row.names=FALSE)



}
