# E_step
estep <- function(datac, theta){
  # compute the expectations of indicators
  d11 <- vapply( 1:nrow(datac), d11.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d12 <- vapply( 1:nrow(datac), d12.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d13 <- vapply( 1:nrow(datac), d13.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d21 <- vapply( 1:nrow(datac), d21.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d22 <- vapply( 1:nrow(datac), d22.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d23 <- vapply( 1:nrow(datac), d23.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  # compute the expectations of sojourn time
  s11 <- vapply( 1:nrow(datac), s11.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s12 <- vapply( 1:nrow(datac), s12.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s13 <- vapply( 1:nrow(datac), s13.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s21 <- vapply( 1:nrow(datac), s21.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s22 <- vapply( 1:nrow(datac), s22.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s23 <- vapply( 1:nrow(datac), s23.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  df.new = data.frame(id = datac$id, d11, d12, d13, d21, d22, d23, s11, s12, s13, s21, s22, s23)
  return(df.new)
}

estep <- function(datac, theta){
  d11 <- vapply( 1:nrow(datac), d11.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d12 <- vapply( 1:nrow(datac), d12.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d13 <- vapply( 1:nrow(datac), d13.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d21 <- vapply( 1:nrow(datac), d21.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d22 <- vapply( 1:nrow(datac), d22.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  d23 <- vapply( 1:nrow(datac), d23.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s11 <- vapply( 1:nrow(datac), s11.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s12 <- vapply( 1:nrow(datac), s12.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s13 <- vapply( 1:nrow(datac), s13.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s21 <- vapply( 1:nrow(datac), s21.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s22 <- vapply( 1:nrow(datac), s22.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  s23 <- vapply( 1:nrow(datac), s23.fn.s, theta = theta, FUN.VALUE = numeric(1), datac = datac)
  df.new = data.frame(id = datac$id, d11, d12, d13, d21, d22, d23, s11, s12, s13, s21, s22, s23)
  return(df.new)
}

mstep.pen <- function(datac, df.new, lambda, penalty, theta, theta.mle){
  # create a pseudo data set
  df.ps <- lapply(1:nrow(df.new), function(i) {
    df <- df.new[i,]
    data.frame(
      id = df$id,
      d = as.numeric(df[, c("d11", "d12", "d13", "d21", "d22", "d23")]),
      piece = rep(1:3, 2),
      type = rep(1:2, each = 3),
      s = as.numeric(df[, c("s11", "s12", "s13", "s21", "s22", "s23")])
    )
  })
  df.ps <- do.call(rbind, df.ps)
  head(df.ps)
  df.ps <- merge(df.ps, datac[, c("id", paste0("x", 1:100))])
  # only keep obs with s > 0
  df.ps <- df.ps[df.ps$s > 0, ]
  # generate indicators
  pieces <- matrix(0, nrow = nrow(df.ps), ncol = (npiece - 1))
  for (k in 2:npiece) {
    pieces[, k - 1] <- ifelse(df.ps$piece == k, 1, 0)
  }
  colnames(pieces) <- c("p2", "p3")
  types <- ifelse(df.ps$type == 2, 1, 0
  )
  # generate the design matrix
  f <- as.formula(d ~ pieces * types + as.factor(type):(. - id - s - type - piece) - 1)
  glm.x <- model.matrix(f, df.ps)
  colnames(glm.x)
  # rearrange the design matrix
  glm.x <- glm.x[, c("piecesp2", "piecesp3", "types",  "piecesp2:types", "piecesp3:types", paste0("as.factor(type)1:x", 1:100), paste0("as.factor(type)2:x", 1:100))]
  # colnames(glm.x)
  if (penalty == "glasso"){
    penalty.index <-  c(rep(NA, 6), rep(1:p, 2))
    fit <- grplasso(x = cbind(1, glm.x), y = as.numeric(df.ps$d), index = penalty.index, offset=log(df.ps$s), lambda = lambda, model = PoissReg(), adaptive.w = rep(1, p), weights = rep(nrow(glm.x)/nrow(datac), nrow(glm.x)))
    cf <- coef(fit)
    # transform into theta.new
    coef.cov <- cf[-c(1:6)]
    current.beta1 <- coef.cov[1:100]
    current.beta2 <- coef.cov[100 + c(1:100)]
    current.beta <- c(current.beta1, current.beta2)
    current.baseline1 <- cf[1] + c(0, cf[2:3])
    current.baseline2 <- current.baseline1 + cf[4] + c(0, cf[5:6])
    cur.lam <- exp(c(current.baseline1, current.baseline2))
    (theta.new <- c(cur.lam, current.beta1, current.beta2))
  }else if (penalty == "aglasso"){
    penalty.index <- c(rep(NA, 6), rep(1:p, 2)) # in grplasso, nonpenalized coefficients are marked with NA while penalty index defines group
    cf1 = theta # use the previous estimate to construct weights
    cf1[cf1 == 0] <- 10e-5
    group.norm <- c(sqrt(rowsum(cf1[-(1:6)] ^ 2, group = rep(1:p, 2))))
    fit <- grplasso(x=cbind(1, glm.x), y = as.numeric(df.ps$d), index = penalty.index, offset=log(df.ps$s), lambda = lambda, model = PoissReg(), center = F, adaptive.w = 1/group.norm, weights = rep(nrow(glm.x)/nrow(datac), nrow(glm.x)))
    cf <- coef(fit)
    coef.cov <- cf[-c(1:6)]
    current.beta1 <- coef.cov[1:100]
    current.beta2 <- coef.cov[100 + c(1:100)]
    current.beta <- c(current.beta1, current.beta2)
    current.baseline1 <- cf[1] + c(0, cf[2:3])
    current.baseline2 <- current.baseline1 + cf[4] + c(0, cf[5:6])
    cur.lam <- exp(c(current.baseline1, current.baseline2))
    (theta.new <- c(cur.lam, current.beta1, current.beta2))
  }else if (penalty == "lasso"){
    penalty.factor <- rep(1, ncol(glm.x))
    nopenalty.index = 1:5
    penalty.factor[nopenalty.index] <- 0
    fit <- glmnet(x=glm.x, y=df.ps$d, weights = rep(nrow(glm.x)/nrow(datac), nrow(glm.x)), family="poisson", offset=log(df.ps$s), penalty.factor = penalty.factor, alpha = 1, lambda = lambda)
    cf <- coef(fit)
    # transform into theta.new
    coef.cov <- cf[-c(1:6)]
    current.beta1 <- coef.cov[1:100]
    current.beta2 <- coef.cov[100 + c(1:100)]
    current.baseline1 <- cf[1] + c(0, cf[2:3])
    current.baseline2 <- current.baseline1 + cf[4] + c(0, cf[5:6])
    cur.lam <- exp(c(current.baseline1, current.baseline2))
    (theta.new <- c(cur.lam, current.beta1, current.beta2))
  }else if (penalty == "alasso"){
    # drop the intercept
    penalty.factor <- rep(1, ncol(glm.x))
    nopenalty.index = 1:5
    penalty.factor[nopenalty.index] <- 0
    cf <- theta
    coef.cov <- cf[-c(1:6)]
    current.beta1 <- coef.cov[1:100]
    current.beta2 <- coef.cov[100 + c(1:100)]
    current.beta <- c(current.beta1, current.beta2)
    # modify = 0 to some larger value 10 -5
    current.beta[current.beta == 0] <- 10 ^ (-5)
    # penalty factor for alasso
    pf <- 1 / abs(current.beta)
    # do not penalize 1:6
    penalty.factor <- c(rep(0, 5), pf)
    fit <- glmnet(x=glm.x, y=df.ps$d, weights = rep(nrow(glm.x)/nrow(datac), nrow(glm.x)), family="poisson", offset = log(df.ps$s), penalty.factor = penalty.factor, alpha = 1, lambda = lambda)
    cf <- coef(fit)
    # transform into theta.new
    coef.cov <- cf[-c(1:6)]
    current.beta1 <- coef.cov[1:100]
    current.beta2 <- coef.cov[100 + c(1:100)]
    current.beta <- c(current.beta1, current.beta2)
    current.baseline1 <- cf[1] + c(0, cf[2:3])
    current.baseline2 <- current.baseline1 + cf[4] + c(0, cf[5:6])
    cur.lam <- exp(c(current.baseline1, current.baseline2))
    (theta.new <- c(cur.lam, current.beta1, current.beta2))
  }else if (penalty == "no"){ # this option without any penalty is to compute the MLE
    fit <- glm.fit(x = cbind(1, glm.x), y = as.numeric(df.ps$d), weights = rep(nrow(glm.x) / nrow(datac), nrow(glm.x)),
        family = quasipoisson(link = "log"), offset = as.numeric(log(df.ps$s)))
    cf <- coef(fit)
    # transform into theta.new
    coef.cov <- cf[-c(1:6)]
    current.beta1 <- coef.cov[1:100]
    current.beta2 <- coef.cov[100 + c(1:100)]
    current.baseline1 <- cf[1] + c(0, cf[2:3])
    current.baseline2 <- current.baseline1 + cf[4] + c(0, cf[5:6])
    cur.lam <- exp(c(current.baseline1, current.baseline2))
    (theta.new <- c(cur.lam, current.beta1, current.beta2))
  }
  # clean up
  pieces <- NULL; y <- NULL; glm.x <- NULL; fit <- NULL; cf <- NULL
  return(theta.new)
}

em.pen2 <- function(theta.start, theta.mle, lambda, datac, penalty){
  cur.theta <- theta.start
  iter = 0
  tol = 9999
  while(tol > 10e-6){
    cat(" iter", iter)
    iter <- iter + 1
    pre.theta <- cur.theta
    df.new <- estep(datac = datac, theta = pre.theta)
    cur.theta <- mstep.pen(datac = datac, df.new, lambda, penalty = penalty, theta = pre.theta)
    # log-transform the baseline intensities
    cur.theta2 <- c(log(cur.theta[1:6]), cur.theta[-c(1:6)])
    pre.theta2 <- c(log(pre.theta[1:6]), pre.theta[-c(1:6)])
    dif.theta <- abs((cur.theta2 - pre.theta2))
    tol <- max(dif.theta)
    cat("tol = ", tol)
    if ( iter > 100 ) { break }
  }
  out <- NULL
  out$tol  <- tol
  out$iter <- iter
  out$theta <- cur.theta
  print(out)
  return(out)
}

