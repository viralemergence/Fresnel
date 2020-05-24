# Fresnel

## Predicting wildlife hosts of betacoronavirus for SARS-CoV-2 sampling prioritisation

This project uses a suite of seven predictive models to predict likely betacoronavirus hosts for sampling prioritisation. The initial report of its findings have been preprinted here: https://www.biorxiv.org/content/10.1101/2020.05.22.111344v1

The scripts for the project are located in the folder `R` and are structured as follows:

## Getting data and running predictive models:
- `00_Fresnel Master.R` contains all source code in order, and should run as an entity straight out of the box.
- `00a_Downloading Data Repos.R` will download the data repos from github, ***including the up-to-date list of known betacoronavirus hosts***.   
-- They will appear in the folder `Github/Repos`.
- `00b_Downloading Model Repos.R` will download the model repos from github.

- The next stage of the fresnel master code will run each model code and output some CSVs containing the results.

## Summarising and presenting the results:
- `01_Fresnel Import.R` will summarise the output CSVs and combine them into one data frame containing the compared model ranks.
- `02_Phylofactor.R` will partition the results into their phylogenetic groups.
- `03a_Bat Figures.R`, `03b_Mammal Figures.R`, and `03c_Albery Figures.R` will plot the other results (except the maps).
