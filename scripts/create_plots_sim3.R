# create and save plots
library(tidyverse)
library(tidyquant)
library(ggdist)

# Create plots directory if it doesn't exist
if (!dir.exists("plots/Sim3/")) {
  dir.create("plots/Sim3/", recursive = TRUE)
}

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
    MDu = abs(PMP1u_H - BESu), # mean average percentage difference
    MSDu = PMP1u_H - BESu, # mean signed difference
    MDc = abs(PMP1c_H - BESc), # mean average percentage difference
    MSDc = PMP1c_H - BESc # mean signed difference
  )

#instead of imputation we switch to a threshhold in the denominator. pmax(PMP1u_H, 1e-6) -> prevents division by 0, no infinite and NaN values

# Sim_3$MAPDu[is.na.data.frame(Sim_3$MAPDu)] <- 0
# max_mapdu <- max(Sim_3$MAPDu[is.finite(Sim_3$MAPDu)])
# Sim_3$MAPDu[is.infinite(Sim_3$MAPDu)] <- max_mapdu
# Sim_3$MAPDc[is.na.data.frame(Sim_3$MAPDc)] <- 0
# max_mapdc <- max(Sim_3$MAPDc[is.finite(Sim_3$MAPDc)])
# Sim_3$MAPDc[is.infinite(Sim_3$MAPDc)] <- max_mapdc

Sim_3_agg <- Sim_3 %>%
  group_by(n_sample, c, d) %>%
  summarise(PMP1u_H = mean(PMP1u_H),
            PMP1c_H = mean(PMP1c_H),
            BESc = mean(BESc),
            BESu = mean(BESu),
            MDu = mean(MDu),
            MDc = mean(MDc),
            MSDu = mean(MSDu),
            MSDc = mean(MSDc))

#############################################################################
## Lineplot MAPD: Bayes Factor tested against the unconstrained Hypothesis ##
#############################################################################

# lineplot_MAPD_unconstrained <- Sim_3_agg %>% filter(d != 0) %>% ggplot() +
#   geom_line(aes(
#     x = as.factor(n_sample),
#     y = MAPDu,
#     color = as.factor(d),
#     group = 1
#   )) +
#   labs(x = "Sample Size (Total)", y = "MAPD") +
#   ggtitle("MAPD: Tested against unconstrained Hypothesis") +
#   scale_color_discrete(guide = "none") +
#   theme_bw() +
#   facet_grid(c ~ d) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave(paste0("plots/Sim3/s3_lineplot_MAPD_unconstrained",".pdf"), lineplot_MAPD_unconstrained, 
#        width = 21, height = 29.7, units = "cm", dpi = 300)

##########################################################################
## Lineplot MAPD: Bayes Factor tested against the complement Hypothesis ##
##########################################################################

# lineplot_MAPD_complement <- Sim_3_agg %>% filter(d != 0) %>% ggplot() +
#   geom_line(aes(
#     x = as.factor(n_sample),
#     y = MAPDc,
#     color = as.factor(d),
#     group = 1
#   )) +
#   labs(x = "Sample Size (Total)", y = "MAPD") +
#   ggtitle("MAPD: Tested against complement Hypothesis") +
#   scale_color_discrete(guide = "none") +
#   theme_bw() +
#   facet_grid(c ~ d) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(paste0("plots/Sim3/s3_lineplot_MAPD_complement",".pdf"), lineplot_MAPD_complement, 
#        width = 21, height = 29.7, units = "cm", dpi = 300)


##########################################################################
## Lineplot MAPD combined ##
##########################################################################

lineplot_MD_combined <- Sim_3_agg %>%
  filter(d != 0) %>%
  pivot_longer(
    cols = c(MDu, MDc),
    names_to = "hypothesis",
    values_to = "MD"
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
    y = MD,
    color = hypothesis,
    linetype = hypothesis,
    group = hypothesis
  )) +
  geom_line(linewidth = 0.6) +
  facet_grid(c ~ d) +
  labs(
    x = "Sample Size (Total)",
    y = "MD",
    linetype = "Tested hypothesis",
    title = "MD across sample size",
    subtitle = "Unconstrained vs Complement hypothesis"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(paste0("plots/Sim3/s3_lineplot_MD_combined",".pdf"), lineplot_MD_combined, 
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
    var = "MSDc",
    fill_var = "ind_comp",
    title = "Bayesian Evidence Synthesis against Complement",
    x_label = "Mean Signed Difference against Complement",
    name = "MSDc_complement"
  ),
  list(
    var = "MSDu",
    fill_var = "ind_comp",
    title = "Bayesian Evidence Synthesis against Unconstrained",
    x_label = "Mean Signed Difference against Unconstrained",
    name = "MSDu_unconstrained"
  )
)

# Iterate through all combinations
for (n in sample_sizes) {
  for (corr in correlations) {
    for (eff in effect_sizes) {
      
      # Filter data for current combination
      filtered_data <- Sim_3 %>% 
        filter(n_sample == n & c == corr & d == eff)
      
      # Skip if no data for this combination
      if (nrow(filtered_data) == 0) {
        next
      }
      
      # Create each plot type
      for (config in plot_configs) {
        
        p <- filtered_data %>%
          ggplot(aes(x = .data[[config$var]], 
                     #fill = factor(.data[[config$fill_var]]), 
                     #color = factor(.data[[config$fill_var]])
                     )) +
          
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
            #fill = "Hypothesis Indicator",
            #color = "Hypothesis Indicator"
          ) +
          theme(
            strip.text = element_text(size = 11, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
        
        # Create filename
        filename <- paste0("plots/Sim3/", config$name, 
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