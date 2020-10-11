# sementesR: PONTOS INICIAIS DISTINTOS 
# nCL: QUANTIDADE DE CLASSES LATENTES
# nManifestas: QUANTIDADE DE VARIÁVEIS MANIFESTAS
# nMC: TAMANHOS AMOSTRA
# nRep: QUANTIDADE DE REPETIÇÕES PARA CADA COMBINAÇÃO DE TAMANHO AMOSTRAL E PONTO INICIAL
source("/media/bgrillob/DATABASE/aMestrado Estatística/Disciplinas/T2 - Computacional/TrabalhoFinal/00_gerarDadosLCA.R")
source("/media/bgrillob/DATABASE/aMestrado Estatística/Disciplinas/T2 - Computacional/TrabalhoFinal/00_motorLCA.R")
source("/media/bgrillob/DATABASE/aMestrado Estatística/Disciplinas/T2 - Computacional/TrabalhoFinal/00_funcaoComparacao.R")

# DEFINIR CRITERIOS SIMULAÇÕES
set.seed(123)
sementesMC <- sample(seq(10 ** 7), size = 5)
nCL <- c(2,3)
nManifestas <- c(2,3)
nMC <- c(100, 200)
nRep <- seq(100)

dfCombinacao <- data.frame(expand.grid(N = nMC, Semente = sementesMC, Classes = nCL, Manifestas = nManifestas, Repeticao = nRep))
l
# SIMULACAO: R-BASE ----
# dfSimulacao <- vector("list")
# t0 <- proc.time()
# for ( w in seq(nrow(dfCombinacao)) ) {
#   # SIMULACAO
#   dfSimulacao[[w]] <- funcaoComparacao(
#     seedR = dfCombinacao$Semente[w], 
#     lcR = dfCombinacao$Classes[w], 
#     mnR = dfCombinacao$Manifestas[w], 
#     nR = dfCombinacao$N[w]
#   )
#   # MARCADOR TEMPO
#   if (w %% 1000 == 0) {
#     difTempo <- proc.time() - t0
#     print(  data.frame(Simulacao = w, TempoDecorridoMinutos = difTempo[3] / 60, PercConcluido = w / nrow(dfCombinacao))  )
#   }
# }

# SIMULACAO: PARALELO ----
library(parallel)
nucleos <- parallel::detectCores() - 1

dfMcLapply <- vector("list", nrow(dfCombinacao))
for (w in seq_along(dfMcLapply)) {
  dfMcLapply[[w]] <- dfCombinacao[w, ]
}

t0 <- proc.time()
dfMcLapply <- mclapply(dfMcLapply, function(x) {
  funcaoComparacao(
    seedR = x$Semente, lcR = x$Classes, 
    mnR = x$Manifestas, nR = x$N
  )
}, mc.cores = nucleos)
proc.time() - t0


# VERIFICAÇÃO PROPRIEDADES ----
library(tidyverse)
comparacao <- dfMcLapply %>%
  bind_rows() %>%
  group_by(N, ClassesLatentesReal, Manifestas) %>%
  mutate(
      # ERRO CLASSIFICAÇÃO
    E_ErroClasseLCA = ClassesLatentesReal != ClassesLatentesEstimada,
    E_ErroClasseBIC = ClassesLatentesReal != ClassesLatentesBIC,
    ErroClasse = ClassesLatentesEstimada - ClassesLatentesReal,
    ErroClasseBIC = ClassesLatentesEstimada - ClassesLatentesBIC,
      # VEROSSIMILHANÇA
    DifLL = logVerossimEstimada - logVerossimReal,
    StLL = (logVerossimEstimada - logVerossimReal) / sd(logVerossimEstimada)
  ) 
      # VERIFICAÇÃO PROPRIEDADES: PLOT ERRO QUANTIDADE CLASSES ----
comparacao %>%
  group_by(N, ClassesLatentesReal, Manifestas) %>%
  summarise(
    PropErroLCA = mean(E_ErroClasseLCA),
    PropErroBIC = mean(E_ErroClasseBIC)
  ) %>% View

dfMcLapply %>%
  bind_rows() %>%
  ggplot(., aes(x = ClassesLatentesEstimada, fill = as.factor(ClassesLatentesReal))) +
    geom_histogram(stat = "count") +
    # geom_vline() +
    facet_wrap(ClassesLatentesReal~N, scales = "free_y") +
    theme_bw()

      # VERIFICAÇÃO PROPRIEDADES: VEROSSIMILHANÇA MAXIMIZADA? ----
ggplot(comparacao, aes(x = StLL, fill = as.factor(N))) +
    geom_density(alpha = 0.3) +
    geom_vline(xintercept = 0) +
    # scale_x_continuous(limits = c(-6, 6)) +
    facet_wrap(~ClassesLatentesReal, scales = "free_y") +
    theme_bw()


ggplot(comparacao, aes(y = ErroClasse))

      # VERIFICAÇÃO PROPORCOES: SOMA DA DIFERENÇA ABSOLUTA ----
dfAME <- dfMcLapply %>% 
  bind_rows() %>% 
  purrr::transpose() %>%
  lapply(., function(x) {
    real <- sort(as.numeric(unlist(strsplit(x$probClasses, split = "_"))))
    estimada <- sort(as.numeric(unlist(strsplit(x$probClassesEstimada, split = "_"))))
    difAbs <- sum(abs((real - estimada))) / length(real)
    
    data.frame(
      N = x$N, Manifestas = x$Manifestas, ClassesLatentesReal = x$ClassesLatentesReal,
      AME = difAbs
    )
  }) %>%
  bind_rows()


ggplot(dfAME, aes(x = AME, fill = as.factor(N))) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ClassesLatentesReal) +
  theme_bw()

# TESTE VERIFICAR ----
w <- 33
la <- funcaoComparacao(
  seedR = dfCombinacao$Semente[w], 
  lcR = dfCombinacao$Classes[w], 
  mnR = dfCombinacao$Manifestas[w], 
  nR = dfCombinacao$N[w], analitico = TRUE
)

X <- la$dadosSimulados$df[, -1]
X <- X+1
poLCA(cbind(X1, X2) ~ 1, data = X, nclass = 2)$llik
