library(MASS)
library(tidyverse)
library(BFpack)
source("function.R")


# Set Parameters
h_s1 <- "mu_s > mu_m"
h_s2 <- "mu_m > mu_f"
h_s3 <- "mu_f > mu_t"
H <- "mu_s > mu_m > mu_f > mu_t"

nsim <- 1e4



# Simulate Data Complete Hypothesis
t1 <- Sys.time()
data_comp <- simulate(nsim = nsim,
                      study = 3,
                      n_sample = c(25, 75, 50, 150, 100, 300, 500, 1500, 5000, 15000),
                      d = c(0, 0.2, 0.5, 0.8),
                      c = c(0, 0.2, 0.5, 0.8))


data_comp <- data_comp %>% # add hypotheses
  mutate(H = H) %>% # compute BF
  rowwise() %>%
  mutate(BF1u = BF(x = s_mu, 
                   Sigma = s_Sigma/n_sample, 
                   n = n_sample, 
                   hypothesis = H)$BFtu_confirmatory[[1]]) %>%
  select(-c(s_mu, s_Sigma))

t2 <- Sys.time()
t2-t1 #4.9h

# Calculate PMPs
data_comp <-  data_comp %>%
  mutate(PMP1u = BF1u/(BF1u + 1))

agg_comp <- data_comp %>%
  group_by(d, c, n_sample, study) %>%
  summarise(BF1u = mean(BF1u),
            PMP1u = mean(PMP1u))

save(data_comp, agg_comp, file = ".../data/data_comp_10k.RData")
rm(data_comp, agg_comp)

# Simulate Data Partial Hypotheses
t1 <- Sys.time()
data_part <- simulate(nsim = nsim,
                      study = 1:3,
                      n_sample = c(25, 50, 100, 500, 5000),
                      d = c(0, 0.2, 0.5, 0.8),
                      c = c(0, 0.2, 0.5, 0.8))


data_part <- data_part %>% # add hypotheses
  mutate(H = case_when(
    study == 1 ~ h_s1,
    study == 2 ~ h_s2,
    study == 3 ~ h_s3)) %>% #compute BF
  rowwise() %>%
  mutate(BF1u = BF(x = s_mu, 
                   Sigma = s_Sigma/n_sample, 
                   n = n_sample, 
                   hypothesis = H)$BFtu_confirmatory[[1]]) %>%
  select(-c(s_mu, s_Sigma))


t2 <- Sys.time()
t2-t1 # 4h

# Calculate PMPs and do BES
data_part <- data_part %>%
  mutate(PMP1u = BF1u/(BF1u + 1)) %>% 
  group_by(d,c, nsim, n_sample) %>%
  mutate(BES_PMP1u = cumprod(PMP1u) / (cumprod(PMP1u) + cumprod(1-PMP1u)))

agg_part <- data_part %>%
  group_by(d, c, n_sample, study) %>%
  summarise(BF1u = mean(BF1u),
            PMP1u = mean(PMP1u),
            BES_PMP1u = mean(BES_PMP1u))

save(data_part, agg_part, file = ".../data/data_part_10k.RData")
rm(data_part, agg_part)
