## simulating fake data

## functions
cov_matrix <- function(n, sigma2, rho){
    m <- matrix(nrow = n, ncol = n)
    m[upper.tri(m)] <- rho
    m[lower.tri(m)] <- rho
    diag(m) <- 1
    (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

T <- 50
S <- 51
current_T <- T - round(runif(1, 3, 25))
mu_b_walk_cov_matrix <- cov_matrix(S, sigma2 = 0.02^2, rho = 0.3)
error_cov_matrix <- cov_matrix(S, sigma2 = 0.01^2, 0.5)


mu_b <- matrix(NA, ncol = S, nrow = T)

mu_b[1, ] <- rbeta(S, 10, 10)
for (t in 2:T){
  mu_b[t, ] <- MASS::mvrnorm(1, mu_b[t - 1, ], Sigma = mu_b_walk_cov_matrix)
}

## plot
plot <- FALSE
if (plot){
  colnames(mu_b) <- paste0("S", seq(1, S))
  t <- 1:T
  df <- as.data.frame(cbind(mu_b, t))
  df <- df %>% gather(key = "S", value = "support", -t)
  ggplot(df) +
    geom_line(aes(x = t, y = support, color = S))
}

## data
N <- 1.5e3
national_prob <- 0.3
national <- rbinom(N, 1, prob = national_prob)
state <- ifelse(national == 0, sample(1:S, N, replace = TRUE), 0)




