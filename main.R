#' - Script citation:
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picolli, Rolf Simoes.
#' Source code for: SITS - Quality control and class noise reduction (July 2020)
#' 
#' Available at: 
#'
#' - Description:
#' This R script was used to generate the results presented on paper. 
#' Details of the methodology are implemented in SITS package.
#'
#' - Reference paper:
#' Lorena Santos, Karine Ferreira, Gilberto Camara, Michelle Picolli, Rolf Simoes.
#' Quality control and class noise reduction of satellite
#' image time series
#' Originally submitted to Remote sensing of environment
#'
#' - Script Usage:
#' Before run this script, open it on any editor or R IDE (e.g. RStudio) to
#' inform the input parameters, in the section `Input Params`.
#' To install the required packages, see section `Installation` below.
#'
#' - Disclaimer:
#' This program is free software: you can redistribute it and/or modify
#' it under the terms of the GNU General Public License as published by
#' the Free Software Foundation, either version 3 of the License, or
#' (at your option) any later version.
#'
#' This program is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU General Public License for more details.
#'
#' You should have received a copy of the GNU General Public License
#' along with this program.  If not, see <https://www.gnu.org/licenses/>.


#----------------------#
##### Installation #####
#----------------------#

# install.packages('devtools')
# install.packages('kohonen')
# install.packages('dplyr')
# install.packages('rgdal')
# install.packages('sp')
# devtools::install_github("e-sensing/sits")


library(sits)
library(kohonen)
library(dplyr)
library(rgdal)
library(sp)
library(magrittr)

source("./functions/point_to_shape.R")
source("./functions/analyze_samples.R")


input_data.tb <- readRDS("./data/input_data.rds")

# Show the number of samples by class
sits::sits_labels(input_data.tb)   

# call the function were the methodology were implemented (SOM + Bayesian)
som_class_noise_reduce.tb <-
  sits::sits_som_map(
    input_data.tb,
    grid_xdim = 20,
    grid_ydim = 20,
    alpha = c(0.5, 0.01),
    rlen = 100,
    distance = "euclidean",
    iterations = 1
  )

#plot SOM map
plot(som_class_noise_reduce.tb)

som_output <- som_class_noise_reduce.tb$samples_output.tb

# Measure the confusion between the samples classes
# Function to show the confusion among the sample classes

cluster_overall <-
  sits::sits_som_evaluate_cluster(som_class_noise_reduce.tb)

confusion_by_samples.tb <- cluster_overall$mixture_samples_by_class
plot(cluster_overall)



## -----------------------------------------------------------------------------
## -------------- Removing and analyzing samples class noise -------------------
## -----------------------------------------------------------------------------

# The fuction sits_som_clean_samples clean the samples directly, but in the paper
# want to explore step by step the percentage of samples that were kept, removed
# and analyzed.


remove_data_set.tb <- dplyr::filter(som_output, conditional_prob < 0.60)

keep_data_set.tb <-
  dplyr::filter(som_output, conditional_prob >= 0.60 &
           posterior_prob >= 0.60)

make_analysis_data_set.tb <-
  dplyr::filter(som_output, conditional_prob >= 0.60 &
           posterior_prob < 0.60)

# Check the number of samples that are kept, removed
# and need to make an analysis

sits::sits_labels(remove_data_set.tb)
sits::sits_labels(keep_data_set.tb)
sits::sits_labels(make_analysis_data_set.tb)

## -----------------------------------------------------------------------------
## -----------------------      Make analysis     ------------------------------
## -----------------------------------------------------------------------------

#Get informations about each neuron
neuron_sample_data <- som_class_noise_reduce.tb$statistics_samples$samples_t

labels_to_analyse <- sits::sits_labels(make_analysis_data_set.tb)$label

#Join the information about the samples probabilities and the neurons information
make_analysis.tb <- make_analysis_data_set.tb %>% dplyr::inner_join(neuron_sample_data, by = "id_sample")


#Analyze the neurons and samples by class
Fallow_Cotton <- analyze_samples(make_analysis.tb, "Fallow_Cotton")
Millet_Cotton <- analyze_samples(make_analysis.tb, "Millet_Cotton")
Pasture <- analyze_samples(make_analysis.tb, "Pasture")
Rocky_Savanna <- analyze_samples(make_analysis.tb, class ="Rocky_Savanna")
Savanna <- analyze_samples(make_analysis.tb, class ="Savanna")
Dense_Woodland <- analyze_samples(make_analysis.tb, class ="Dense_Woodland")
Silviculture <- analyze_samples(make_analysis.tb, class ="Silviculture")
Soy_Corn <- analyze_samples(make_analysis.tb, "Soy_Corn")
Soy_Fallow <- analyze_samples(make_analysis.tb, "Soy_Fallow")
Savanna_Parkland <- analyze_samples(make_analysis.tb, class ="Savanna_Parkland")


#prepare the samples to plot in qgis to verify the spatial location
point_to_shape (make_analysis.tb, name_file = "samples_to_analyze_info")

## -----------------------      Discard samples     ------------------------------

#Discard samples through neurons
discard_samples <-
  dplyr::filter(
    make_analysis.tb,
    id_neuron %in% c(17, 37, 352, 392,18, 36, 302, 208, 289, 381, 388, 390, 351, 312, 235, 15)
  )$id_sample


discarded_samples.tb <- dplyr::filter(make_analysis_data_set.tb, (id_sample %in% discard_samples))

#see the number of discarded samples
sits::sits_labels(discarded_samples.tb)

#samples that were kept in dataset
after_analysis <- dplyr::filter(make_analysis_data_set.tb, !(id_sample %in% discard_samples))

after_analysis <-
  dplyr::select(
    after_analysis,
    id_sample,
    latitude,
    longitude,
    start_date,
    end_date,
    label,
    cube,
    time_series,
    conditional_prob,
    posterior_prob
  )

#Join the samples that were kept in dataset - Output dataset
output_data_set.tb <- rbind(keep_data_set.tb, after_analysis)

#see the number of the samples by class in the output dataset
sits::sits_labels(output_data_set.tb)


## -----------------------------------------------------------------------------
## ------------------------------  Validation  ---------------------------------
## -----------------------------------------------------------------------------


# -------------------------------  Input data ------------------------------

input_data_conf_rfor.tb <-
  sits::sits_kfold_validate(
    input_data.tb,
    folds = 5,
    multicores = 1,
    ml_method = sits_rfor()
  )

print("== Confusion Matrix = RFOR =======================")
input_conf_rfor.mx <- sits_conf_matrix(input_data_conf_rfor.tb)

# -------------------------------  Output data ------------------------------
output_data_conf_rfor.tb <-
  sits::sits_kfold_validate(
    output_data_set.tb,
    folds = 5,
    multicores = 1,
    ml_method = sits_rfor()
  )

print("== Confusion Matrix = RFOR =======================")
output_conf_rfor.mx <- sits_conf_matrix(output_data_conf_rfor.tb)




saveRDS(input_data, file = "input_data.rds")
save(input_data, som_class_noise_reduce.tb, output_data_set.tb, file = "Paper_results.RData")
