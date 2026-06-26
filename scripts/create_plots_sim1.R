# create and save plots
library(tidyverse)
library(ggdist)

# Consistent Bayesian colour palette (colorblind-friendly)
col_primary   <- "#8B1A1A"   # dark red / maroon
col_secondary <- "#2166AC"   # steel blue
col_box_fill  <- "#EACBCB"   # light rose

# Create plots directory if it doesn't exist
if (!dir.exists("plots/Sim1/")) {
  dir.create("plots/Sim1/", recursive = TRUE)
}

load("data/Sim_1.RData")
df_with_na <- Sim_1[apply(is.na(Sim_1), 1, any), ]
# only NA in BESc, bc of 0 in one of the PMPs

cumprod(c(0.013,0.000001,1)) / (cumprod(c(0.013,0.000001,1)) + cumprod(c(1-0.013,1-00.000001,1-1)))

Sim_1$BESc <- ifelse(is.nan(Sim_1$BESc), 1, Sim_1$BESc)

Sim_1$n_sample <- Sim_1$n_sample*3

# Add indicator or partial
Sim_1$ind_p_all <- ifelse(Sim_1$ind_p1 == 1 & Sim_1$ind_p2 == 1 & Sim_1$ind_p3 == 1, 1, 0)

Sim_1 <- Sim_1 %>%
  group_by(n_sample, d, c) %>%
  mutate(
    MDu = abs(PMP1u_H - BESu), # mean average percentage difference
    MSDu = PMP1u_H - BESu, # mean signed difference
    MDc = abs(PMP1c_H - BESc), # mean average percentage difference
    MSDc = PMP1c_H - BESc # mean signed difference
  )

Sim_1_agg <- Sim_1 %>%
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
## Plot MSD: Bayes Factor tested against the unconstrained Hypothesis ##
#############################################################################

# Sim_1 %>% filter(n_sample == 75 & d == 0.2 & c == 0.2) %>%
#   ggplot(aes(x = MSDu)) +
#   stat_dots(
#     aes(y = 0),
#     side = "top",
#     scale = 0.8,
#     alpha = 0.6,
#     dotsize = 1.5
#   ) +
#   geom_boxplot(
#     aes(y = -0.15),
#     width = 0.05,
#     outlier.shape = 16,
#     outlier.size = 1,
#     alpha = 0.7
#   ) +
#   scale_fill_tq() +
#   scale_color_tq() +
#   theme_tq() +
#   labs(title = "title", x = "xlabel", y = "") +
#   theme(
#     strip.text = element_text(size = 11, face = "bold"),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     plot.title = element_text(hjust = 0.5)
#   )



##########################################################################
## Lineplot MSD: Bayes Factor tested against the complement Hypothesis ##
##########################################################################

# p <- Sim_1 %>%
#   ggplot(aes(x = MDc)) +
#   stat_dots(
#     aes(y = 0),
#     side = "top",
#     scale = 0.8,
#     alpha = 0.6,
#     dotsize = 1.5
#   ) +
#   geom_boxplot(
#     aes(y = -0.15),
#     width = 0.05,
#     outlier.shape = 16,
#     outlier.size = 1,
#     alpha = 0.7
#   ) +
#   scale_fill_tq() +
#   scale_color_tq() +
#   theme_tq() +
#   labs(title = "title", x = "xlabel", y = "") +
#   theme(
#     strip.text = element_text(size = 11, face = "bold"),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     plot.title = element_text(hjust = 0.5)
#   )
# 
# ggsave(filename = "plots/Sim1/MD_c.pdf", plot = p)

##########################################################################
## Lineplot MD Combined ##
##########################################################################

lineplot_MD_combined <- Sim_1_agg %>%
  #filter(d != 0) %>%
  pivot_longer(
    cols = c(MDu, MDc),
    names_to = "hypothesis",
    values_to = "MD"
  ) %>%
  mutate(
    hypothesis = recode(
      hypothesis,
      MDu = "vs. Unconstrained",
      MDc = "vs. Complement"
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
  scale_color_manual(
    name = "Comparison",
    values = c("vs. Unconstrained" = col_primary, "vs. Complement" = col_secondary)
  ) +
  scale_linetype_manual(
    name = "Comparison",
    values = c("vs. Unconstrained" = "solid", "vs. Complement" = "longdash")
  ) +
  facet_grid(c ~ d, labeller = labeller(
    c = as_labeller(function(x) paste0("rho[i]==", x), label_parsed),
    d = as_labeller(function(x) paste0("mu[i]==", x), label_parsed)
  )) +
  labs(
    x = "Sample Size (Total)",
    y = "MD"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(paste0("plots/Sim1/s1_lineplot_MD_combined",".pdf"), lineplot_MD_combined, 
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
      filtered_data <- Sim_1 %>% 
        filter(n_sample == n & c == corr & d == eff)
      
      # Skip if no data for this combination
      if (nrow(filtered_data) == 0) {
        next
      }
      
      # Create each plot type
      for (config in plot_configs) {
        p <- filtered_data %>%
          ggplot(aes(x = .data[[config$var]])) +
          geom_vline(
            xintercept = 0,
            linetype = "dashed",
            color = "grey40",
            linewidth = 0.4,
            alpha = 0.8
          ) +
          
          stat_dots(
            aes(
              y = 0,
              fill = factor(.data[[config$fill_var]], levels = c(0, 1), labels = c("0", "1")),
              color = factor(.data[[config$fill_var]], levels = c(0, 1), labels = c("0", "1"))
            ),
            side = "top",
            scale = 0.8,
            alpha = 0.6,
            dotsize = 1.5
          ) +
          
          geom_boxplot(
            aes(y = -0.15),
            fill = col_box_fill,
            color = col_primary,
            width = 0.05,
            outlier.shape = 16,
            outlier.size = 1,
            outlier.alpha = 0.1,
            alpha = 0.7
          ) +
          scale_fill_manual(
            name = "Hypothesis indicator (ind_comp)",
            values = c("0" = col_secondary, "1" = col_primary)
          ) +
          scale_color_manual(
            name = "Hypothesis indicator (ind_comp)",
            values = c("0" = col_secondary, "1" = col_primary)
          ) +
          
          theme_minimal(base_size = 11) +
          labs(
            title = bquote(n[i] == .(n) ~ ", " ~ rho[i] == .(corr) ~ ", " ~ mu[i] == .(eff)),
            x = config$x_label,
            y = ""
          ) +
          coord_cartesian(xlim = c(0, 1)) +
          theme(
            strip.text = element_text(size = 11, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom"
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

