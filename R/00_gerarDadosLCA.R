# OBJETIVO: GERAR DADOS ALEATÓRIOS DE LCA COM TAMANHOS DISTINTOS
# INPUTS:
  # nObs: QUANTIDADE DE OBSERVAÇÕES A SEREM GERADAS
  # nClasses: QUANTIDADE DE CLASSES LATENTES
  # nManifestas: QUANTIDADE DE VARIÁVEIS MANIFESTAS
  # probs: LISTA COM PROBABILIDADES, PADRÃO É NULO. CASO PRECISE SER SEMPRE IGUAL, AJUSTAR SEMENTE
  # probClasses: VETOR COM PROPORÇÕES DE CADA CLASSE LATENTE
  # semente: SEED PARA GERAR SEMPRE AS MESMAS PROBABILIDADES
gerarDadosLCA <- function(nObs, nClasses = 3, nManifestas = 4, probs = NULL, probClasses = NULL, semente = 15081991) {
  # GERAR LISTA COM PROBS (CASO NÃO TENHA SIDO ESPECIFICADA)
  if( is.null(probs) ) {
    set.seed(semente)
    sementes <- sample(seq(10**6), size = nManifestas)
    probs <- vector("list", nManifestas)
    for (w in seq(nManifestas)) {
      set.seed(sementes[w])                                                     # GERAR SEMPRE MESMAS PROBS
      probTemp <- matrix(runif(nClasses * 2), nrow = nClasses, ncol = 2)        # 2 POR SER MANIFESTA BINÁRIA
      probTemp <- probTemp / apply(probTemp, 1, sum)                            # APLICAR RESTRIÇÃO P(.) = 1
      probs[[w]] <- probTemp
    }
  }
  # GERAR PROPORÇÃO DE CADA CLASSE
  if( is.null(probClasses) ) {
    set.seed(semente)
    probClasses <- runif(nClasses)
    probClasses <- probClasses / sum(probClasses)
  }
  
  # GERAR MANIFESTAS + CLASSES
  dfRetorno <- data.frame(Z = sample(x = seq(nClasses), size = nObs, replace = TRUE, prob = probClasses))
  dfManifestas <- matrix(nrow = nObs, ncol = nManifestas)
  for ( w in seq(nrow(dfManifestas)) ) {
    for ( y in seq(nManifestas) ) {
      dfManifestas[w, y] <- rbinom(n = 1, size = 1, prob = probs[[y]][dfRetorno$Z[w], 2] )  # COLUNA 2 DA MATRIZ PROBS É X = 1
    }
  }
  colnames(dfManifestas) <- paste("X", seq(ncol(dfManifestas)), sep = "")
  dfRetorno <- data.frame(dfRetorno, dfManifestas)

  # RESULTADO
  resultado <- list(df = dfRetorno, probClasse = probClasses, listaProbs = probs)
return(resultado)
}

