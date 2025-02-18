library(MASS)
library(tidyverse)
library(BFpack)
source("scripts/function.R")

set.seed(1337)

# Set Parameters
h_s1 <- "mu_s > mu_m"
h_s2 <- "mu_m > mu_f"
h_s3 <- "mu_f > mu_t"
H <- "mu_s > mu_m > mu_f > mu_t"

nsim <- 2500



# Simulate Data Complete Hypothesis
t1 <- Sys.time()
data <- simulate(nsim = nsim,
                      study = 1:6,
                      n_sample = c(25, 75, 50, 150, 100, 300, 500, 1500),
                      d = c(0.2),
                      c = c(0.2)) %>%
  # add hypotheses
  mutate(H_comp = H,
         H_part = case_when(
           study == 1 ~ h_s1,
           study == 2 ~ h_s2,
           study == 3 ~ h_s3,
           study == 4 ~ h_s1,
           study == 5 ~ h_s2,
           study == 6 ~ h_s3)) %>% 
  # compute BFs
  rowwise() %>%
  mutate(BF_comp = list(BF(x = s_mu, 
                           Sigma = s_Sigma/n_sample, 
                           n = n_sample, 
                           hypothesis = H_comp)),
         BF1u_comp = BF_comp$BFtu_confirmatory[[1]],
         PMP1c_comp = BF_comp$PHP_confirmatory[[1]],
         BF_part = list(BF(x = s_mu, 
                           Sigma = s_Sigma/n_sample, 
                           n = n_sample, 
                           hypothesis = H_part)),
         BF1u_part = BF_part$BFtu_confirmatory[[1]],
         PMP1c_part = BF_part$PHP_confirmatory[[1]])


# indicate of complete hypothesis is true
data$ind <- NA
for (i in 1:nrow(data)) {
  data$ind[i] <- ifelse(data$s_mu[[i]][1] > data$s_mu[[i]][2] && data$s_mu[[i]][2] > data$s_mu[[i]][3] && data$s_mu[[i]][3] > data$s_mu[[i]][4], 1, 0)
  #IF DATA STUY == 1 -> remove
}

data <- data %>%
  dplyr::select(-c(s_mu, s_Sigma, BF_comp, BF_part)) %>%
  mutate(PMP1u_comp = BF1u_comp/(BF1u_comp + 1),
         PMP1u_part = BF1u_part/(BF1u_part + 1)) %>%
  group_by(nsim, n_sample) %>%
  mutate(BES_PMP1c_part = cumprod(PMP1c_part) / (cumprod(PMP1c_part) + cumprod(1-PMP1c_part)),
         BES_PMP1u_part = cumprod(PMP1u_part) / (cumprod(PMP1u_part) + cumprod(1-PMP1u_part))) %>%
  ungroup()

data_tmp <- data %>% filter(study %in% c(5,6)) %>%
  group_by(nsim, n_sample) %>%
  mutate(BES_PMP1c_comp = cumprod(PMP1c_comp) / (cumprod(PMP1c_comp) + cumprod(1-PMP1c_comp)),
         BES_PMP1u_comp = cumprod(PMP1u_comp) / (cumprod(PMP1u_comp) + cumprod(1-PMP1u_comp))) %>%
  ungroup()

data <- data %>%
  dplyr::left_join(
    data_tmp %>% dplyr::select(nsim, n_sample, study, BES_PMP1c_comp, BES_PMP1u_comp),
    by = dplyr::join_by(nsim, n_sample, study)
  )


t2 <- Sys.time()
t2-t1 #2.5h

save(data, file = "data/sim_2c6pe_02_2500.Rdata")
