cv.lme <- function (data, lmefit, cost = function(y, yhat) mean((y - yhat)^2), 
    K = n) 
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
    cost.0 <- cost(lme.y, fitted(lmefit))
    ms <- max(s)
    CV <- 0
    Call <- lmefit$call
    for (i in seq_len(ms)) {
        j.out <- seq_len(n)[(s == i)]
        j.in <- seq_len(n)[(s != i)]
        Call$data <- data[j.in, , drop = FALSE]
        d.lme <- eval.parent(Call)
        p.alpha <- n.s[i]/n
        cost.i <- cost(lme.y[j.out], predict(d.lme, data[j.out, 
            , drop = FALSE], type = "response"))
        if (is.na(cost.i))
          browser() #print(summary(d.lme))
        CV <- CV + p.alpha * cost.i
        cost.0 <- cost.0 - p.alpha * cost(lme.y, predict(d.lme, 
            data, type = "response"))
    }
    list(call = call, K = K, delta = c(CV, CV + cost.0), seed = seed)
}