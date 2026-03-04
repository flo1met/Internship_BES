# create and save plots
library(tidyverse)
library(tidyquant)
library(ggdist)

load("data/Sim_3.RData")

anyNA(Sim_3)
df_with_na <- Sim_3[apply(is.na(Sim_3), 1, any), ]

# "impute" all NaN with 1 because:
cumprod(c(1,0,1)) / (cumprod(c(1,0,1)) + cumprod(c(1-1,1-0,1-1)))

cumprod(c(1,0.0001,1)) / (cumprod(c(1,0.0001,1)) + cumprod(c(1-1,1-0.0001,1-1)))

# correct?
Sim_3$BESc <- ifelse(is.nan(Sim_3$BESc), 1, Sim_3$BESc)

Sim_3$n_sample <- Sim_3$n_sample*3

# Add indicator or partial
Sim_3$ind_p_all <- ifelse(Sim_3$ind_p1 == 1 & Sim_3$ind_p2 == 1 & Sim_3$ind_p3 == 1, 1, 0)

Sim_3 <- Sim_3 %>%
  group_by(n_sample, d, c) %>%
  mutate(
    MAPDu = abs(PMP1u_H - BESu), # mean average percentage difference
    MSDu = PMP1u_H - BESu, # mean signed difference
    MAPDc = abs(PMP1c_H - BESc) , # mean average percentage difference
    MSDc = PMP1c_H - BESc # mean signed difference
  )


Sim_3_agg <- Sim_3 %>%
  group_by(n_sample, c, d) %>%
  summarise(PMP1u_H = mean(PMP1u_H),
            PMP1c_H = mean(PMP1c_H),
            BESc = mean(BESc),
            BESu = mean(BESu),
            MAPDu = mean(MAPDu),
            MAPDc = mean(MAPDc),
            MSDu = mean(MSDu),
            MSDc = mean(MSDc))

#############################################################################
## Lineplot MAPD: Bayes Factor tested against the unconstrained Hypothesis ##
#############################################################################

lineplot_MAPD_unconstrained <- Sim_3_agg %>% filter(d != 0) %>% ggplot() +
  geom_line(aes(
    x = as.factor(n_sample),
    y = MAPDu,
    color = as.factor(d),
    group = 1
  )) +
  labs(x = "Sample Size (Total)", y = "MAPD") +
  ggtitle("MAPD: Tested against unconstrained Hypothesis") +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  facet_grid(c ~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################################
## Lineplot MAPD: Bayes Factor tested against the complement Hypothesis ##
##########################################################################

lineplot_MAPD_complement <- Sim_3_agg %>% filter(d != 0) %>% ggplot() +
  geom_line(aes(
    x = as.factor(n_sample),
    y = MAPDc,
    color = as.factor(d),
    group = 1
  )) +
  labs(x = "Sample Size (Total)", y = "MAPD") +
  ggtitle("MAPD: Tested against complement Hypothesis") +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  facet_grid(c ~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################################################
## Lineplot MAPD combined ##
##########################################################################

lineplot_MAPD_combined <- Sim_3_agg %>%
  filter(d != 0) %>%
  pivot_longer(
    cols = c(MAPDu, MAPDc),
    names_to = "hypothesis",
    values_to = "MAPD"
  ) %>%
  mutate(
    hypothesis = recode(
      hypothesis,
      MAPDu = "Unconstrained",
      MAPDc = "Complement"
    )
  ) %>%
  ggplot(aes(
    x = as.factor(n_sample),
    y = MAPD,
    color = hypothesis,
    linetype = hypothesis,
    group = hypothesis
  )) +
  geom_line(linewidth = 0.6) +
  facet_grid(c ~ d) +
  labs(
    x = "Sample Size (Total)",
    y = "MAPD",
    linetype = "Tested hypothesis",
    title = "MAPD across sample size",
    subtitle = "Unconstrained vs Complement hypothesis"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






###### Sim 1


load("data/Sim_1.RData")
df_with_na <- Sim_1[apply(is.na(Sim_1), 1, any), ]
# only NA in BESc, bc of 0 in one of the PMPs

# "impute" all NaN with 1 because:
cumprod(c(0.6,0,1)) / (cumprod(c(0.6,0,1)) + cumprod(c(1-0.6,1-0,1-1)))

cumprod(c(0.6,0.0000001,1)) / (cumprod(c(0.6,0.0000001,1)) + cumprod(c(1-0.6,1-0.0000001,1-1)))

# correct?
Sim_1$BESc <- ifelse(is.nan(Sim_1$BESc), 1, Sim_1$BESc)

Sim_1$n_sample <- Sim_1$n_sample*3

# Add indicator or partial
Sim_1$ind_p_all <- ifelse(Sim_1$ind_p1 == 1 & Sim_1$ind_p2 == 1 & Sim_1$ind_p3 == 1, 1, 0)

Sim_1 <- Sim_1 %>%
  group_by(n_sample, d, c) %>%
  mutate(
    MAPDu = abs(PMP1u_H - BESu), # mean average percentage difference
    MSDu = PMP1u_H - BESu, # mean signed difference
    MAPDc = abs(PMP1c_H - BESc), # mean average percentage difference
    MSDc = PMP1c_H - BESc # mean signed difference
  )

# Set NaNs in MAPDc to 0 bc they only accured if abs((PMP1c_H - BESc) / PMP1c_H) = abs((0 - 0) / 0)
Sim_1$MAPDc <- ifelse(is.nan(Sim_1$MAPDc), 0, Sim_1$MAPDc)
# Important note: all d = 0 end in Inf for MAPDc (as expected)

Sim_1_agg <- Sim_1 %>%
  group_by(n_sample, c, d) %>%
  summarise(PMP1u_H = mean(PMP1u_H),
            PMP1c_H = mean(PMP1c_H),
            BESc = mean(BESc),
            BESu = mean(BESu),
            MAPDu = mean(MAPDu),
            MAPDc = mean(MAPDc),
            MSDu = mean(MSDu),
            MSDc = mean(MSDc))

#############################################################################
## Lineplot MAPD: Bayes Factor tested against the unconstrained Hypothesis ##
#############################################################################

lineplot_MAPD_unconstrained_1 <- Sim_1_agg %>% filter(d != 0) %>% ggplot() +
  geom_line(aes(
    x = as.factor(n_sample),
    y = MAPDu,
    color = as.factor(d),
    group = 1
  )) +
  labs(x = "Sample Size (Total)", y = "MAPD") +
  ggtitle("MAPD: Tested against unconstrained Hypothesis") +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  facet_grid(c ~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################################
## Lineplot MAPD: Bayes Factor tested against the complement Hypothesis ##
##########################################################################

lineplot_MAPD_complement_1 <- Sim_1_agg %>% filter(d != 0) %>% ggplot() +
  geom_line(aes(
    x = as.factor(n_sample),
    y = MAPDc,
    color = as.factor(d),
    group = 1
  )) +
  labs(x = "Sample Size (Total)", y = "MAPD") +
  ggtitle("MAPD: Tested against complement Hypothesis") +
  scale_color_discrete(guide = "none") +
  theme_bw() +
  facet_grid(c ~ d) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########################################################################
## Lineplot MAPD Combined ##
##########################################################################

lineplot_MAPD_combined_1 <- Sim_1_agg %>%
  filter(d != 0) %>%
  pivot_longer(
    cols = c(MAPDu, MAPDc),
    names_to = "hypothesis",
    values_to = "MAPD"
  ) %>%
  mutate(
    hypothesis = recode(
      hypothesis,
      MAPDu = "Unconstrained",
      MAPDc = "Complement"
    )
  ) %>%
  ggplot(aes(
    x = as.factor(n_sample),
    y = MAPD,
    color = hypothesis,
    linetype = hypothesis,
    group = hypothesis
  )) +
  geom_line(linewidth = 0.6) +
  facet_grid(c ~ d) +
  labs(
    x = "Sample Size (Total)",
    y = "MAPD",
    linetype = "Tested hypothesis",
    title = "MAPD across sample size",
    subtitle = "Unconstrained vs Complement hypothesis"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
