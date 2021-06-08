reload_Oz_squamate_trained_model <- function(model_file, Oz_squamate_phylo_x) {
  
  phylo_mat <- Oz_squamate_phylo_x %>%
    dplyr::arrange(phylo_id) %>%
    dplyr::select(-species, -phylo_id) %>%
    as.matrix()
  
  model <- deepSDF_phylo_model(phylo_mat = phylo_mat,
                               dropout_rate = 0.1, ridge_lambda = 0.1)
  
  keras::load_model_weights_hdf5(model, model_file)
  
  model
  
}