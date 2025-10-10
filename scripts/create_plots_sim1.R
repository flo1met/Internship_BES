# create and save plots
library(tidyverse)
library(tidyquant)
library(ggdist)

# Create plots directory if it doesn't exist
if (!dir.exists("plots/Sim1/")) {
  dir.create("plots/Sim1/", recursive = TRUE)
}

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
    MAPDu = abs((PMP1u_H - BESu) / PMP1u_H) * 100, # mean average percentage difference
    MSDu = PMP1u_H - BESu, # mean signed difference
    MAPDc = abs((PMP1c_H - BESc) / PMP1c_H) * 100, # mean average percentage difference
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

lineplot_MAPD_unconstrained <- Sim_1_agg %>% filter(d != 0) %>% ggplot() +
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

ggsave(paste0("plots/Sim1/s1_lineplot_MAPD_unconstrained",".pdf"), lineplot_MAPD_unconstrained, 
       width = 21, height = 29.7, units = "cm", dpi = 300)

##########################################################################
## Lineplot MAPD: Bayes Factor tested against the complement Hypothesis ##
##########################################################################

lineplot_MAPD_complement <- Sim_1_agg %>% filter(d != 0) %>% ggplot() +
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
ggsave(paste0("plots/Sim1/s1_lineplot_MAPD_complement",".pdf"), lineplot_MAPD_complement, 
       width = 21, height = 29.7, units = "cm", dpi = 300)

#########################
## Distplots all cases ##
#########################

# Define parameters
sample_sizes <- c(75, 150, 300, 1500)
correlations <- c(0, 0.2, 0.5, 0.8)
effect_sizes <- c(0, 0.2, 0.5, 0.8)

# Define plot configurations
plot_configs <- list(
  list(
    var = "BESc",
    fill_var = "ind_comp",
    title = "Bayesian Evidence Synthesis against Complement",
    x_label = "Posterior Model Probabilities against Complement",
    name = "BESc_complement"
  ),
  list(
    var = "PMP1c_H",
    fill_var = "ind_p_all",
    title = "PMPs against Complement",
    x_label = "Posterior Model Probabilities",
    name = "PMP1c_complement"
  ),
  list(
    var = "BESu",
    fill_var = "ind_comp",
    title = "Bayesian Evidence Synthesis against Unconstrained",
    x_label = "Posterior Model Probabilities",
    name = "BESu_unconstrained"
  ),
  list(
    var = "PMP1u_H",
    fill_var = "ind_p_all",
    title = "PMPs against Unconstrained",
    x_label = "Posterior Model Probabilities",
    name = "PMP1u_unconstrained"
  )
)

# Iterate through all combinations
for (n in sample_sizes) {
  for (corr in correlations) {
    for (eff in effect_sizes) {
      
      # Filter data for current combination
      filtered_data <- Sim_1 %>% 
        filter(n_sample == n & c == corr & d == eff)
      
      # Skip if no data for this combination
      if (nrow(filtered_data) == 0) {
        next
      }
      
      # Create each plot type
      for (config in plot_configs) {
        
        p <- filtered_data %>%
          ggplot(aes(x = .data[[config$var]], 
                     fill = factor(.data[[config$fill_var]]), 
                     color = factor(.data[[config$fill_var]]))) +
          
          stat_dots(
            aes(y = 0),
            side = "top",
            scale = 0.8,
            alpha = 0.6,
            dotsize = 1.5
          ) +
          
          geom_boxplot(
            aes(y = -0.15),
            width = 0.05,
            outlier.shape = 16,
            outlier.size = 1,
            alpha = 0.7
          ) +
          
          scale_fill_tq() +
          scale_color_tq() +
          theme_tq() +
          labs(
            title = paste0(config$title, 
                           "\nn = ", n, ", c = ", corr, ", d = ", eff),
            x = config$x_label,
            y = "",
            fill = "Hypothesis Indicator",
            color = "Hypothesis Indicator"
          ) +
          theme(
            strip.text = element_text(size = 11, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
        
        # Create filename
        filename <- paste0("plots/Sim1/", config$name, 
                           "_n", n, "_c", corr, "_d", eff, ".pdf")
        
        # Save plot
        ggsave(filename, plot = p,
               width = 7, height = 4.5, 
               units = "in", 
               device = "pdf")
        
        cat("Saved:", filename, "\n")
      }
    }
  }
}

cat("\nAll plots saved successfully!\n")