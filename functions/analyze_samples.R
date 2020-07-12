# Code to analyze the samples that must be analyzed by class
# This function return the neurons that must be analyzed
# and plot the samples that are allocated in each sample to
# help the analyses.

#------ ------ Parameters ---------------
# data   - Samples to analyze
# class  - class label
#----------------------------------------

analyze_samples <- function(data, class = "NULL")
{
  class_to_analyse <- dplyr::filter(data, label == class) 
  
  #get the neurons that must be analyzed
  summary_analyses <- unique(dplyr::select(class_to_analyse, id_neuron, neuron_label, conditional_prob, posterior_prob))
  
  id_neuron_analysis <- summary_analyses$id_neuron
  
  #plot the samples by neurons to analyze the spectral patterns
  for (i in 1:length(id_neuron_analysis))
  {
    id_samples_analysis <- dplyr::filter(class_to_analyse, id_neuron == id_neuron_analysis[i])$id_sample
    
    plot(sits::sits_select_bands(
      dplyr::filter(class_to_analyse, id_sample %in% id_samples_analysis),
      ndvi
    ))
    
  }
  
  return(id_neuron_analysis)
  
}
