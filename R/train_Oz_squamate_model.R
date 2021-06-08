##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Oz_squamate_phylo_train_dat
##' @param Oz_squamate_phylo_x
train_Oz_squamate_model <- function(Oz_squamate_phylo_train_dat,
                                    Oz_squamate_phylo_x,
                                    save_file) {

  phylo_mat <- Oz_squamate_phylo_x %>%
    dplyr::arrange(phylo_id) %>%
    dplyr::select(-species, -phylo_id, -root) %>%
    as.matrix()
  
  model <- deepSDF_phylo_model(phylo_mat = phylo_mat,
                               dropout_rate = 0.1, ridge_lambda = 0.01)
  
  loss_func <- SDF_loss_mae()
  
  model %>%
    keras::compile(optimizer = "adam",
                   loss = loss_func)
  
  early_stop <- keras::callback_early_stopping(monitor = 'loss',
                                               patience = 10,
                                               restore_best_weights = TRUE)
  
  plateau <- callback_reduce_lr_on_plateau(monitor = 'loss',
                                           factor = 0.5,
                                           patience = 5,
                                           verbose = 0,
                                           mode = 'auto',
                                           min_delta = 0,
                                           cooldown = 0,
                                           min_lr = 0)
  
  model %>%
    keras::fit(x = Oz_squamate_phylo_train_dat$train$x,
               y = Oz_squamate_phylo_train_dat$train$y,
               batch_size = 100000L,
               validation_data = unname(Oz_squamate_phylo_train_dat$validation),
               callbacks = list(early_stop, plateau),
               verbose = 2, view_metrics = FALSE,
               epochs = 10)
  
  #keras::save_model_weights_hdf5(model, save_file)
  
  model$get_weights()
  
}
