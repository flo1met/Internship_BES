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
data_comp <- simulate(nsim = nsim,
                      study = 2:3,
                      n_sample = c(25, 75, 50, 150, 100, 300, 500, 1500),
                      d = c(0.2),
                      c = c(0.2))
# indicate of complete hypothesis is true
data_comp$ind <- NA
for (i in 1:nrow(data_comp)) {
  data_comp$ind[i] <- ifelse(data_comp$s_mu[[i]][1] > data_comp$s_mu[[i]][2] && data_comp$s_mu[[i]][2] > data_comp$s_mu[[i]][3] && data_comp$s_mu[[i]][3] > data_comp$s_mu[[i]][4], 1, 0)
}



data_comp <- data_comp %>% # add hypotheses
  mutate(H = H) %>% # compute BF
  rowwise() %>%
  mutate(BF_result = list(BF(x = s_mu, 
                             Sigma = s_Sigma/n_sample, 
                             n = n_sample, 
                             hypothesis = H)),
         BF1u = BF_result$BFtu_confirmatory[[1]],
         PMP1c = BF_result$PHP_confirmatory[[1]]) %>%
  select(-c(s_mu, s_Sigma, BF_result))

t2 <- Sys.time()
t2-t1 #2.5h

# Calculate PMPs
data_comp <-  data_comp %>%
  mutate(PMP1u = BF1u/(BF1u + 1)) %>% 
  group_by(d,c, nsim, n_sample) %>%
  mutate(BES_PMP1c = cumprod(PMP1c) / (cumprod(PMP1c) + cumprod(1-PMP1c)),
         BES_PMP1u = cumprod(PMP1u) / (cumprod(PMP1u) + cumprod(1-PMP1u)))
  

agg_comp <- data_comp %>%
  group_by(d, c, n_sample, study) %>%
  summarise(BF1u = mean(BF1u),
            PMP1c = mean(PMP1c),
            PMP1u = mean(PMP1u),
            BES_PMP1c = mean(BES_PMP1c),
            BES_PMP1u = mean(BES_PMP1u))

save(data_comp, agg_comp, file = "data/data_comp_2500_2c_6p.RData")
rm(data_comp, agg_comp)

# Simulate Data Partial Hypotheses
t1 <- Sys.time()
data_part <- simulate(nsim = nsim,
                      study = 1:6,
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
    study == 3 ~ h_s3,
    study == 4 ~ h_s1,
    study == 5 ~ h_s2,
    study == 6 ~ h_s3)) %>% #compute BF
  rowwise() %>%
  mutate(BF_result = list(BF(x = s_mu, 
                             Sigma = s_Sigma/n_sample, 
                             n = n_sample, 
                             hypothesis = H)),
         BF1u = BF_result$BFtu_confirmatory[[1]],
         PMP1c = BF_result$PHP_confirmatory[[1]]) %>%
  select(-c(s_mu, s_Sigma, BF_result))


t2 <- Sys.time()
t2-t1 # 2h

# Calculate PMPs and do BES
data_part <- data_part %>%
  mutate(PMP1u = BF1u/(BF1u + 1)) %>% 
  group_by(d,c, nsim, n_sample) %>%
  mutate(BES_PMP1c = cumprod(PMP1c) / (cumprod(PMP1c) + cumprod(1-PMP1c)),
         BES_PMP1u = cumprod(PMP1u) / (cumprod(PMP1u) + cumprod(1-PMP1u)))

agg_part <- data_part %>%
  group_by(d, c, n_sample, study) %>%
  summarise(BF1u = mean(BF1u),
            PMP1c = mean(PMP1c),
            PMP1u = mean(PMP1u),
            BES_PMP1c = mean(BES_PMP1c),
            BES_PMP1u = mean(BES_PMP1u))

save(data_part, agg_part, file = "data/data_part_2500_2c_6p.RData")
rm(data_part, agg_part)
