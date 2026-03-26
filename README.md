# EntoInsights [![License: CC BY-NC 4.0](https://img.shields.io/badge/License-CC_BY_NC_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)

The **EntoInsights** R package provides a range of functions to support users of the Automated Monitoring of Insects (AMI) and LepiSense systems. Its functionality broadly assists users who wish to analyse classification datasets, as well as partners reviewing and retrieving their uploaded image and acoustic datasets.

## Installing the package

The package does not have any non-R dependencies and can be installed simply by running:

```r
# Install devtools if needed
install.packages("devtools")

# Install EntoInsights from GitHub
devtools::install_github("AMI-system/EntoInsights")

# Load the package
library(EntoInsights)
```

## Functionality

The current functionality of the package allows:
* Downloading raw data (images, audio, or ultrasound)
    * List deployments in the object store and retrieve their associated metadata (requires API username and password).
    * Download and process snapshot images, audio, and ultrasound recordings (requires a credentials file).
    * List files uploaded to deployments (requires a credentials file).
* Data preparation
    * Wrangle data into a standardised format for further analysis.
    * Plot the operational period of the deployment using a calendar-style plot.
    * Extract datetime information from filenames.
    * Clean UK-based classification records using the **record cleaner** recording confidence rules.
    * Filter to only include moths or high-confidence moth species predictions.
* Data summarisation 
    * Generate summaries of number of detections (activity) or species richness over the deployment period.
    * Calculate deployment operational metrics (nights/days active).
    * Generate top species rankings by total detections or nights detected.
    * Compute summary statistics for detections and species per day/night.
* Data visualisation
    * Plot operational periods of devices in various formats (bar, calendar, or tile plots).
    * Create interactive Leaflet maps showing technology deployment locations.
    * Visualise the proportion of moths vs non-moths.
    * Plot number of detections (activity) or species richness over time using line or bubble plots.
* Additional functionalities
    * Extract environmental variables using the NASA POWER API and the R packages **moonlit** and **elevatr**.

For a full list of all functions, run:
```r
help(package = "EntoInsights")
```
The vignette also includes a more in-depth example of the package functionality. The vignette uses the `/data/classifications_df.rda` example dataset.

## Minimal example workflow
This example demonstrates a typical workflow using the included `/data/example_data.rda` example dataset:

```r
# Load the package
library(EntoInsights)

# Load dependencies
library(dplyr)

# Load the example dataset
data(example_data)

# Wrangle the data into a format suitable for all remaining downstream processing steps
wrangled_results <- wrangle_results(results_file = example_data, taxa = "moth", filepath_colname = "image_path", sitename_colname = "deployment_name", latitude_colname = "latitude", longitude_colname = "longitude", topspeciespred_colname = "top_1_species", speciesconf_colname = "top_1_confidence")

# Create a map showing the deployment location
ami_sites <- wrangled_results %>%
  dplyr::select(latitude, longitude) %>%
  dplyr::distinct()

EntoInsights::map(
  latitude  = ami_sites$latitude,
  longitude = ami_sites$longitude
)

# Create a calendar plot showing device operation over the deployment
calendar <- EntoInsights::plot_device_operation_calendar(wrangled_results)

# Make a plot showing the number of detections, which is an indication of insect activity over the deployment period
plot_activity_graph(dataframe = wrangled_results, type = "detections")

# Visualise how many detections are of moths, and how many are of other insects
plot_moths_vs_nonmoths(wrangled_results, moth_nonmoth_classifier = TRUE, order_classifier = FALSE, conf_threshold = 0.8, type = "pie")

# Filter to detections predicted to be a moth with over 0.8 confidence (either by moth vs non-moth or order classifier)
moths_only <- moths_only(wrangled_results, conf_threshold = 0.8)

# Append the record cleaner recording confidence rules to the dataset
wrangled_results_rc <- append_RC_rules(moths_only, ami_sites$latitude, ami_sites$longitude)

# Remove the implausible predictions based on location, date, and identification difficulty (also remove NAs)
refined_wrangled_results_rc <- remove_implausible_predictions(dataframe_with_rc = wrangled_results_rc,
                                                              filter_location = TRUE,
                                                              filter_date = TRUE,
                                                              filter_id_difficulty = TRUE,
                                                              max_id_difficulty = 2,
                                                              keep_na = FALSE)

# Remove species predictions with a confidence score smaller than 0.7
confident_moths <- filter_by_conf_threshold(refined_wrangled_results_rc, conf_threshold = 0.7)

# Make a plot showing the number of species observed, giving an idea of species diversity over the deployment period
plot_activity_graph(dataframe = confident_moths, type = "species")

# At this point, further filtering could be done using the traits database to remove species predictions where there are very few GB records, or to filter moths based on size

# Visualise when the species detected on the most number of nights during the deployment were active
plot_top_species_bubble(confident_moths, type = "nights", n = 8)
```

