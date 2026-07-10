# Bayesian Evidence Synthesis (BES): Aggregating evidence from partially overlapping hypotheses
This repository contains all files and scripts used and created during the "Bayesian Evidence Synthesis (BES): Aggregating evidence from partially overlapping hypotheses" project.

## Repository Structure

* `data/`: Contains the simulated data
* `pots/`: Contains all plots
* `scripts/`: Simulation and analysis scripts

## Reproducing the analysis

If you wish to rerun the full analysis, execute the scripts in the following order:

1. `scripts/Simulations.qmd`: Simulate data and compute BFs
2. `scripts/create_plots_sim1.R`, `scripts/create_plots_sim2.R`, `scripts/create_plots_sim3.R`, `scripts/create_plots_sim4.R`: Creates the plots used for analysis