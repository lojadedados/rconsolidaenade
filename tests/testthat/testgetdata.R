library(rconsolidaenade)
context("Testes do Metodo getData")


test_that("Testando a recuperacao de Dados com o Metodo GetData", {
 # Verifica a existencia de 155 colunas no arquivo
 expect_equal(ncol(getData()),155)

})
