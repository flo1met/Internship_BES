simulate <- function(nsim, study, n_sample, d, c) {
  
  data <- expand_grid(nsim = 1:nsim, n_sample, study, d, c) %>%
    rowwise() %>%
    mutate(
      s_mu = list(setNames(mvrnorm(1, mu = c(4-d, 4-2*d, 4-3*d, 4-4*d),
                                   Sigma = (`diag<-`(matrix(rep(c, 16), nrow = 4, ncol = 4), 1) / n_sample)),
                           c("mu_s", "mu_m", "mu_f", "mu_t"))),
      s_Sigma = list(rWishart(1, df = (n_sample-1), Sigma = (`diag<-`(matrix(rep(c, 16), nrow = 4, ncol = 4), 1)))[,,1]/(n_sample-1))
    )
  
  return(data)
}