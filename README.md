# voi-shiny-dashboard
Interactive VOI explorer built in R Shiny for playing with uncertainty, expected value, and EVPI.
# Value of Information Explorer

An interactive R Shiny dashboard for exploring the basics of value of information (VOI) in health economics.

This app lets users adjust assumptions in a simple toy example and see how uncertainty affects:

- incremental net monetary benefit
- expected value of perfect information (EVPI)
- decision direction across scenarios

## What this app does

The dashboard compares:
- **Standard care**
- **A new treatment**

Users can modify:
- willingness-to-pay threshold
- scenario probabilities
- costs
- QALYs
- population size

The app then calculates:
- scenario-specific incremental net monetary benefit
- expected incremental net monetary benefit
- EVPI per patient
- population EVPI

## Why this is useful

This is a simple way to explore how VOI works and how uncertainty can affect decision-making in health economics.

## How to run locally

1. Open the project in R or RStudio
2. Install required packages if needed:
   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "DT", "scales"))
