cvsub.lme <- function (data, lmefit, cost = function(y, yhat) mean((y - yhat)^2), 
    K = n, cv.subset = TRUE) 
{
    call <- match.call()
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    n <- nrow(data)
    out <- NULL
    if ((K > n) || (K <= 1)) 
        stop("K outside allowable range")
    K.o <- K
    K <- round(K)
    kvals <- unique(round(n/(1L:floor(n/2))))
    temp <- abs(kvals - K)
    if (!any(temp == 0)) 
        K <- kvals[temp == min(temp)][1L]
    if (K != K.o) 
        warning("K has been set to ", K)
    f <- ceiling(n/K)
    s <- boot:::sample0(rep(1L:K, f), n)
    n.s <- table(s)
    lme.y <- getResponse(lmefit)
    cost.0 <- cost(lme.y[cv.subset], fitted(lmefit)[cv.subset])
    ms <- max(s)
    CV <- 0
    Call <- lmefit$call
    for (i in seq_len(ms)) {
        j.out <- seq_len(n)[(s == i)]
        j.in <- seq_len(n)[(s != i)]
        Call$data <- data[j.in, , drop = FALSE]
        d.lme <- eval.parent(Call)
        p.alpha <- n.s[i]/n
        cost.i <- cost(lme.y[j.out & cv.subset], 
		predict(d.lme, data[j.out & cv.subset, , drop = FALSE], 
			type = "response"))
        #if (is.na(cost.i))
        #  browser() #print(summary(d.lme))
        CV <- CV + p.alpha * cost.i
        cost.0 <- cost.0 - p.alpha * cost(lme.y[cv.subset], 
		predict(d.lme, data[cv.subset, , drop=FALSE], 
			type = "response"))
    }
    list(call = call, K = K, delta = c(CV, CV + cost.0), seed = seed)
}

cvsub.glm <- function (data, glmfit, cost = function(y, yhat) mean((y - yhat)^2), 
    K = n, cv.subset = TRUE) 
{
    call <- match.call()
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    n <- nrow(data)
    out <- NULL
    if ((K > n) || (K <= 1)) 
        stop("K outside allowable range")
    K.o <- K
    K <- round(K)
    kvals <- unique(round(n/(1L:floor(n/2))))
    temp <- abs(kvals - K)
    if (!any(temp == 0)) 
        K <- kvals[temp == min(temp)][1L]
    if (K != K.o) 
        warning("K has been set to ", K)
    f <- ceiling(n/K)
    s <- boot:::sample0(rep(1L:K, f), n)
    n.s <- table(s)
    glm.y <- glmfit$y
    cost.0 <- cost(glm.y[cv.subset], fitted(glmfit)[cv.subset])
    ms <- max(s)
    CV <- 0
    Call <- glmfit$call
    for (i in seq_len(ms)) {
        j.out <- seq_len(n)[(s == i)]
        j.in <- seq_len(n)[(s != i)]
        Call$data <- data[j.in, , drop = FALSE]
        d.glm <- eval.parent(Call)
        p.alpha <- n.s[i]/n
        cost.i <- cost(glm.y[j.out & cv.subset], 
		predict(d.glm, data[j.out & cv.subset, , drop = FALSE], 
			type = "response"))
        CV <- CV + p.alpha * cost.i
        cost.0 <- cost.0 - p.alpha * cost(glm.y[cv.subset], 
		predict(d.glm, data[cv.subset, , drop=FALSE], 
			type = "response"))
    }
    list(call = call, K = K, delta = c(CV, CV + cost.0), seed = seed)
}
