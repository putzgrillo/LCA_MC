# E-STEP ----
e_step <- function(dados, nComponentes, theta, pesos) {
  n_rows <- nrow(dados)
  n_cols <- ncol(dados)
  dados <- as.matrix(dados)
  r_numerator <- matrix(0, nrow = n_rows, ncol = nComponentes)
  for (k in seq(nComponentes)) {
    # NUMERADOR
    for (w in seq(n_rows)) {
      r_numerator[w, k] <- prod(dbinom(x = dados[w, ], size = 1, prob = theta[k,])) * pesos[k]
    }
  }
  # DENOMINADOR
  r_denominator <- apply(r_numerator, 1, sum)
  # LOG-VEROSSIMILHANÇA
  logVerossimilhanca <- sum(log(apply(r_numerator, 1, sum)))
  resultado <- list(
    self_responsability = r_numerator / r_denominator,
    logVerossimilhanca = logVerossimilhanca
  )
  
  return(resultado)   # retorna o self_responsability
}


# M STEP ----
m_step <- function(dados, nComponentes, responsability) {
  n_rows <- nrow(dados)
  n_cols <- ncol(dados)
  dados <- as.matrix(dados)
  theta <- matrix(0, nrow = nComponentes, ncol = n_cols)
  
  # PI (PROBABILIDADES CONDICIONAIS)
  pesos <- vector("numeric", nComponentes)
  for (k in seq(nComponentes)) {
    pesos[k] <- sum(responsability[, k]) / nrow(dados)
  }
  
  for (w in seq(nComponentes)) {
    numerator_aux <- matrix(0, nrow = n_rows, ncol = n_cols)
    for (y in seq(n_rows)) {
      numerator_aux[y, ] <- responsability[y, w] * dados[y, ]
    }
    numerator = apply(numerator_aux, 2, sum)
    denominator <- sum(responsability[, w])
    theta[w, ] <- numerator / denominator
  }
  # LISTA RESULTADO
  resultado <- list(
    theta = theta,
    weights = pesos
  )
return(resultado)
}


# FIT ----
doFit <- function(X, nComponentes = 3, maxIter = 100, convergencia = 0.01) {
  # INICIALIZAÇÃO
  peso <- runif(n = nComponentes)
  peso <- peso / sum(peso)
  
  theta <- matrix(runif(n = nComponentes * ncol(X)), nrow = nComponentes, ncol = ncol(X))
  theta <- theta / sum(theta)
  
  # RODAR EM 
  verossimilhanca <- vector("numeric")
  verossimilhanca[1] <- e_step(dados = X, nComponentes = nComponentes, theta = theta, pesos = peso)$logVerossimilhanca
  
  continuar <- TRUE
  etapa <- 1
  while (continuar) {
    expectation <- e_step(dados = X, nComponentes = nComponentes, theta = theta, pesos = peso)
    maximization <- m_step(dados = X, nComponentes = nComponentes, responsability = expectation$self_responsability)
    
    responsa <- expectation$self_responsability
    etapa <- etapa + 1
    verossimilhanca[etapa] <- expectation$logVerossimilhanca
    if (etapa <= 2) {
      continuar <- TRUE
    } else {
      continuar <- abs(verossimilhanca[etapa] - verossimilhanca[etapa - 1]) > convergencia
    }
    
    peso <- maximization$weights
    theta <- maximization$theta
    
    if( etapa > maxIter) {break}
  }
  
  dfCategorizado = data.frame(Z = apply(responsa, 1, function(x) which.max(x)), X)
  
  bic <- nComponentes * log(nrow(X)) - 2 * max(verossimilhanca)       # k*log(n) - 2 * log(L)
  
  resultado <- list(
    theta = theta,
    peso = peso, 
    verossimilhanca = verossimilhanca,
    BIC = bic,
    df = dfCategorizado
  )
  return(resultado)
}

# teste <- doFit(X = df, nComponentes = 4, maxIter = 100)


