#' @title reduz dataset removendo informacoes que nao serao uteis
#' @description Funcao para reduzir o tamanho do Dataset original
#' @author Rodrigo Almeida - rodrigo.almeida@gmail.com
#' @export
reduzDataset <- function() {

  # URL do Dataset original
  urlDatasetOriginal <- "data/microdados_enade_2014.csv"
  colunasDesnecessarias <- c("nu_ano", "co_orgac","co_regiao_curso","nu_idade","tp_sexo","ano_fim_2g",
                             "ano_in_grad","tp_semestre","in_matutino","in_vespertino","in_noturno","id_status",
                             "amostra","tp_inscricao","tp_def_fis","tp_def_vis","tp_def_aud","nu_item_ofg",
                             "nu_item_ofg_z","nu_item_ofg_x","nu_item_ofg_n","vt_gab_ofg_orig","nu_item_oce",
                             "nu_item_oce_z","nu_item_oce_x","nu_item_oce_n","vt_gab_oce_orig","tp_pres",
                             "tp_pr_ger","tp_pr_ob_fg","tp_pr_di_fg","tp_pr_ob_ce","tp_pr_di_ce","tp_sfg_d1",
                             "tp_sfg_d2","tp_sce_d1","tp_sce_d2","tp_sce_d3","vt_ace_ofg",
                             "vt_ace_oce")

  if (is.null(data) || is.null(nrow(data))) {
    data <- subset(read.table(urlDatasetOriginal,header=T,sep=";"), co_grupo == 4006)
  }

  data <- data[,!(names(data) %in% colunasDesnecessarias)]
  save(data, file = "data/data.Rda")

}
