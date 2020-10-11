# OBJETIVO: COMPARAR DESEMPENHO DO LCA COM DADOS SINTÉTICOS ONDE OS PARÂMETROS SÃO CONHECIDOS
  # INPUTS:
    # nR: QUANTIDADE DE OBSERVAÇÕES A SEREM GERADAS
    # lcR: QUANTIDADE DE CLASSES LATENTES
    # mnR: QUANTIDADE DE VARIÁVEIS MANIFESTAS
    # seedR: SEED USADA NA GERAÇÃO ALEATÓRIA (ÚTIL PARA REPRODUCIBILIDADE)
    # escolheR: FLAG QUE DETERMINA SE SELECIONA NUMERO DE CLASSES DE ACORDO COM MENOR BIC
              # SE VERDADEIRO, USA BIC, SE FALSO, USA lcR (ÚTIL QUANDO PENSA-SE NO LCA COMO UM MÉTODO SUPERVISIONADO)
    # analitico: FLAG QUE DETERMINA SE RETORNA OBJETOS SIMULADOS E LCA ESCOLHIDO 
              # EVITAR USAR ISSO EM SIMULAÇÃO DE LARGA ESCALA, POIS ESTOURA A MEMÓRIA RAPIDAMENTE
# CADA ITERAÇÃO
funcaoComparacao <- function(seedR, lcR, mnR, nR, escolherR = FALSE, analitico = FALSE) {
  # INFOS SIMULADAS
  dfLCA <- gerarDadosLCA(nObs = nR, nClasses = lcR, nManifestas = mnR)
  verossimilhancaReal <- e_step(
    dados = dfLCA$df[, -1],
    pesos = dfLCA$probClasse,
    nComponentes = length(dfLCA$probClasse),
    theta = do.call(cbind, lapply(dfLCA$listaProbs, function(x) {x[, 2]}))
  )$logVerossimilhanca
  proporcaoClasses <- paste(round(dfLCA$probClasse, 3), collapse = "_")
  
  # INFOS TESTES
  if ( escolherR ) {
    listLCA <- vector("list")
    for (w in seq(5)) {
      listLCA[[w]] <- doFit(X = dfLCA$df[, -1], nComponentes = w, convergencia = 0.001)
    }
    nClassesBIC <- which.min(unlist(lapply(listLCA, function(x) {x$BIC})))
    nClassesLCA <- which.max(unlist(lapply(listLCA, function(x) {max(x$verossimilhanca)})))
    verossimilhancaEstimada <- max(listLCA[[nClassesLCA]]$verossimilhanca)
  } else {
    listLCA <- doFit(X = dfLCA$df[, -1], nComponentes = lcR, convergencia = 0.001)
    nClassesBIC <- lcR
    nClassesLCA <- lcR
    verossimilhancaEstimada <- max(listLCA$verossimilhanca)
  }
  
  
  # RESULTADOS
  dfComparacao <- data.frame(
    N = nR, ClassesLatentes = lcR, Manifestas = mnR,
    ClassesLatentesReal = lcR, ClassesLatentesEstimada = nClassesLCA, ClassesLatentesBIC = nClassesBIC,
    logVerossimReal = verossimilhancaReal, logVerossimEstimada = verossimilhancaEstimada, EscolheR = escolherR,
    probClasses = proporcaoClasses 
  )
  
  if (analitico) {
    resultado <- list(
      dfComparacao = dfComparacao, dadosSimulados = dfLCA, selectedLCA = listLCA[[nClassesLCA]]
    )
  } else {
    resultado <- dfComparacao
  }
return(resultado)
}
