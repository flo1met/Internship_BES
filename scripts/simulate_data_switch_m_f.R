# Scenario: Switch m and f in partial
# test against full scenario: 
#   - correct hypothesis
#   - wrong hypothesis
#   - null hypotehsis
#   - switch hypothesis

library(MASS)
library(tidyverse)
library(BFpack)
source("function.R")

set.seed(1337)

# Set Parameters
h_s1 <- "mu_s > mu_m"
h_s2 <- "mu_f > mu_m"
h_s3 <- "mu_f > mu_t"

H_correct <- "mu_s > mu_m > mu_f > mu_t"
H_wrong <- "mu_s < mu_m < mu_f < mu_t"
H_switch <- "mu_s > mu_f > mu_m > mu_t"
H_null <- "mu_s = mu_f = mu_m = mu_t"

nsim <- 2500



# Simulate Data Complete Hypothesis
t1 <- Sys.time()
data_comp <- simulate(nsim = nsim,
                      study = 3,
                      n_sample = c(25, 75, 50, 150, 100, 300, 500, 1500),
                      d = c(0.2),
                      c = c(0.2))
# indicate of compete hypothesis is true
data_comp$ind <- NA
for (i in 1:nrow(data_comp)) {
  data_comp$ind[i] <- ifelse(data_comp$s_mu[[i]][1] > data_comp$s_mu[[i]][2] && data_comp$s_mu[[i]][2] > data_comp$s_mu[[i]][3] && data_comp$s_mu[[i]][3] > data_comp$s_mu[[i]][4], 1, 0)
}

data_comp <- data_comp %>% 
  # add hypotheses
  mutate(H_correct = H_correct,
         H_wrong = H_wrong,
         H_switch = H_switch,
         H_null = H_null,) %>% 
  # compute BF
  rowwise() %>%
  mutate(BF_correct = list(BF(x = s_mu, 
                             Sigma = s_Sigma/n_sample, 
                             n = n_sample, 
                             hypothesis = H_correct)), # testing against correct hypothesis
         BF1u_correct = BF_correct$BFtu_confirmatory[[1]],
         PMP1u_correct = BF1u_correct/(BF1u_correct + 1),
         BF1c_correct = BF_correct$BFtu_confirmatory[[1]]/BF_correct$BFtu_confirmatory[[2]],
         PMP1c_correct = BF1c_correct/(BF1c_correct+1),
         BF_wrong = list(BF(x = s_mu, 
                             Sigma = s_Sigma/n_sample, 
                             n = n_sample, 
                             hypothesis = H_wrong)), # testing against completely wrong hypothesis
         BF1u_wrong = BF_wrong$BFtu_confirmatory[[1]],
         PMP1u_wrong = BF1u_wrong/(BF1u_wrong + 1),
         BF1c_wrong = BF_wrong$BFtu_confirmatory[[1]]/BF_wrong$BFtu_confirmatory[[2]],
         PMP1c_wrong = BF1c_wrong/(BF1c_wrong+1),
         BF_switch = list(BF(x = s_mu, 
                            Sigma = s_Sigma/n_sample, 
                            n = n_sample, 
                            hypothesis = H_switch)), # testing against completely wrong hypothesis
         BF1u_switch = BF_switch$BFtu_confirmatory[[1]],
         PMP1u_switch = BF1u_switch/(BF1u_switch + 1),
         BF1c_switch = BF_switch$BFtu_confirmatory[[1]]/BF_switch$BFtu_confirmatory[[2]],
         PMP1c_switch = BF1c_switch/(BF1c_switch+1),
         BF_null = list(BF(x = s_mu, 
                             Sigma = s_Sigma/n_sample, 
                             n = n_sample, 
                             hypothesis = H_null)), # testing against completely wrong hypothesis
         BF1u_null = BF_null$BFtu_confirmatory[[1]],
         PMP1u_null = BF1u_null/(BF1u_null + 1),
         BF1c_null = BF_null$BFtu_confirmatory[[1]]/BF_null$BFtu_confirmatory[[2]],
         PMP1c_null = BF1c_null/(BF1c_null+1)) %>%
  dplyr::select(-c(s_mu, s_Sigma, BF_correct, BF_wrong, BF_switch, BF_null))

t2 <- Sys.time()
t2-t1 #2.5h

# Aggregated DF
agg_comp <- data_comp %>%
  group_by(d, c, n_sample, study) %>%
  summarise(BF1u_correct = mean(BF1u_correct),
            PMP1u_correct = mean(PMP1u_correct),
            BF1c_correct = mean(BF1c_correct),
            PMP1c_correct = mean(PMP1c_correct),
            BF1u_wrong = mean(BF1u_wrong),
            PMP1u_wrong = mean(PMP1u_wrong),
            BF1c_wrong = mean(BF1c_wrong),
            PMP1c_wrong = mean(PMP1c_wrong),
            BF1u_switch = mean(BF1u_switch),
            PMP1u_switch = mean(PMP1u_switch),
            BF1c_switch = mean(BF1c_switch),
            PMP1c_switch = mean(PMP1c_switch),
            BF1u_null = mean(BF1u_null),
            PMP1u_null = mean(PMP1u_null),
            BF1c_null = mean(BF1c_null),
            PMP1c_null = mean(PMP1c_null))

save(data_comp, agg_comp, file = "../data/data_comp_2500_switch_m_f.RData")
rm(data_comp, agg_comp)

# Simulate Data Partial Hypotheses
t1 <- Sys.time()
data_part <- simulate(nsim = nsim,
                      study = 1:3,
                      n_sample = c(25, 50, 100, 500),
                      d = c(0.2),
                      c = c(0.2))

# indicate of complete hypothesis is true
data_part$ind <- NA
for (i in 1:nrow(data_part)) {
  data_part$ind[i] <- ifelse(data_part$s_mu[[i]][1] > data_part$s_mu[[i]][2] && data_part$s_mu[[i]][2] > data_part$s_mu[[i]][3] && data_part$s_mu[[i]][3] > data_part$s_mu[[i]][4], 1, 0)
}

data_part <- data_part %>% # add hypotheses
  mutate(H = case_when(
    study == 1 ~ h_s1,
    study == 2 ~ h_s2,
    study == 3 ~ h_s3)) %>% #compute BF
  rowwise() %>%
  mutate(BF_result = list(BF(x = s_mu, 
                             Sigma = s_Sigma/n_sample, 
                             n = n_sample, 
                             hypothesis = H)),
         BF1u = BF_result$BFtu_confirmatory[[1]],
         PMP1u = BF1u/(BF1u + 1),
         BF1c = BF_result$BFtu_confirmatory[[1]]/BF_result$BFtu_confirmatory[[2]],
         PMP1c = BF1c/(BF1c+1)) %>%
  dplyr::select(-c(s_mu, s_Sigma, BF_result))


t2 <- Sys.time()
t2-t1 # 2h

# Do BES
data_part <- data_part %>%
  group_by(d,c, nsim, n_sample) %>%
  mutate(BES_PMP1c = cumprod(PMP1c) / (cumprod(PMP1c) + cumprod(1-PMP1c)),
         BES_PMP1u = cumprod(PMP1u) / (cumprod(PMP1u) + cumprod(1-PMP1u)))

agg_part <- data_part %>%
  group_by(d, c, n_sample, study) %>%
  summarise(BF1u = mean(BF1u),
            BF1c = mean(BF1c),
            PMP1c = mean(PMP1c),
            PMP1u = mean(PMP1u),
            BES_PMP1c = mean(BES_PMP1c),
            BES_PMP1u = mean(BES_PMP1u))

save(data_part, agg_part, file = "../data/data_part_2500_switch_m_f.RData")
rm(data_part, agg_part)
