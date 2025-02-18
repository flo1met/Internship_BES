#######################################
#### Function to simulate the data ####
#######################################
simulate <- function(nsim, n_sample, d, c) {
  
  data <- expand_grid(nsim = 1:nsim, n_sample, d, c) %>%
    rowwise() %>%
    mutate(
      s_mu = list(setNames(mvrnorm(1, mu = c(4-d, 4-2*d, 4-3*d, 4-4*d),
                                   Sigma = (`diag<-`(matrix(rep(c, 16), nrow = 4, ncol = 4), 1) / n_sample)),
                           c("mu_1", "mu_2", "mu_3", "mu_4"))),
      s_Sigma = list(rWishart(1, df = (n_sample-1), Sigma = (`diag<-`(matrix(rep(c, 16), nrow = 4, ncol = 4), 1)))[,,1]/(n_sample-1))
    )
  return(data)
}

##########################################################################################
#### Function to indicate whether complete and partial hypotheses are true in the data ####
##########################################################################################

addIndicator <- function(Sim_Study, hypothesis) {
  
  # Initialize the new column with NA
  Sim_Study[[paste0("ind_", hypothesis)]] <- NA
  
  # Compute the specified indicator based on hypothesis
  Sim_Study[[paste0("ind_", hypothesis)]] <- sapply(Sim_Study$s_mu, function(mu) {
    if (hypothesis == "comp") {
      return(as.integer(mu[1] > mu[2] && mu[2] > mu[3] && mu[3] > mu[4]))
    } else if (hypothesis == "p1") {
      return(as.integer(mu[1] > mu[2]))
    } else if (hypothesis == "p2") {
      return(as.integer(mu[2] > mu[3]))
    } else if (hypothesis == "p3") {
      return(as.integer(mu[3] > mu[4]))
    }
  })
  return(Sim_Study)
}

#addIndicator <- function(Sim_Study) {
#  
#  Sim_Study$ind_comp <- NA
#  Sim_Study$ind_p1 <- NA
#  Sim_Study$ind_p2 <- NA
#  Sim_Study$ind_p3 <- NA
#  
#  
#  for (i in 1:nrow(Sim_Study)) {
#    Sim_Study$ind_comp[i] <- ifelse(Sim_Study$s_mu[[i]][1] > Sim_Study$s_mu[[i]][2] && Sim_Study$s_mu[[i]][2] > Sim_Study$s_mu[[i]][3] && Sim_Study$s_mu[[i]][3] > Sim_Study$s_mu[[i]][4], 1, 0)
#    
#    Sim_Study$ind_p1[i] <- ifelse(Sim_Study$s_mu[[i]][1] > Sim_Study$s_mu[[i]][2], 1, 0)
#    Sim_Study$ind_p2[i] <- ifelse(Sim_Study$s_mu[[i]][2] > Sim_Study$s_mu[[i]][3], 1, 0)
#    Sim_Study$ind_p3[i] <- ifelse(Sim_Study$s_mu[[i]][3] > Sim_Study$s_mu[[i]][4], 1, 0)
#  }
#  return(Sim_Study)
#}

