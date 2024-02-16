data = read.csv("GMM.csv")

X <- data[,"X"]
K <- data[,"K"]
pi_0_hat <- sum(K==0)/nrow(data)
pi_0_hat
# > pi_0_hat
# [1] 0.94914

mu <- function(X, k) {
    return(sum(X[K==k])/sum(K==k))
}

mu0_hat <- mu(X, 0)
mu1_hat <- mu(X, 1)

mu0_hat
# > mu0_hat
# [1] 49.95767
mu1_hat
# > mu1_hat
# [1] 60.81269

sigma <- function(X, k, mu_k) {
    return(sum((X[K==k] - mu_k)^2)/sum(K==k))
}

var_0_hat = sigma(X, 0, mu0_hat)
var_1_hat = sigma(X, 1, mu1_hat)

var_0_hat
# > var_0_hat
# [1] 99.86054
var_1_hat
# > var_1_hat
# [1] 101.6141