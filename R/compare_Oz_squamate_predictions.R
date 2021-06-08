##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Oz_squamate_trained_model_reload
##' @param Oz_squamate_phylo_x
##' @param Oz_prediction_grid
compare_Oz_squamate_predictions <- function(Oz_squamate_trained_model_reload,
                                            Oz_squamate_phylo_ids,
                                            Oz_prediction_grid,
                                            Oz_squamates) {

  phylo_mat <- Oz_squamate_trained_model_reload$required_data %>%
    dplyr::arrange(phylo_id) %>%
    dplyr::select(-species, -phylo_id, -root) %>%
    as.matrix()
  
  model <- deepSDF_phylo_model(phylo_mat = phylo_mat,
                               dropout_rate = 0.1, ridge_lambda = 0.01)
  
  model$set_weights(Oz_squamate_trained_model_reload$model_weights)
  
  #keras::load_model_weights_hdf5(model, Oz_squamate_trained_model_reload$model_file)
  
  # pred_mod <- reload_Oz_squamate_trained_model(Oz_squamate_trained_model_reload$model_file,
  #                                  Oz_squamate_trained_model_reload$required_data)
  
  test_num <- 1L
  
  test_dat <- list(coord_input = Oz_prediction_grid,
                   phylo_id_input = matrix(test_num, 
                                           nrow = nrow(Oz_prediction_grid), 
                                           ncol = 1))
  test <- predict(model, test_dat)
  test <- model(keras::keras_array(test_dat, "float32"))
  
  prediction_df <- make_prediction_grid(NULL, Oz, return_type = "tibble", use_coords = TRUE)
  test_dat <- list(coord_input = as.matrix(prediction_df),
                   phylo_id_input = matrix(test_num, 
                                           nrow = nrow(prediction_df), 
                                           ncol = 1))
  test <- predict(model, test_dat)
  test_it <- predict_to_sf(model, test_dat, prediction_df)
  
  shape_centroid <- sf::st_centroid(Oz %>% sf::st_union())
  st <- Oz - shape_centroid
  
  norms <- st %>%
    sf::st_coordinates() %>%
    .[ , 1:2] %>%
    apply(1, function(x) sqrt(sum(x^2)))
  
  scaler <- max(norms)
  
  centrer <- shape_centroid %>% sf::st_coordinates() %>%
    as.vector()
  
  
  plot((Oz - centrer) / scaler)
  plot(test_it, add = TRUE)
  
  
  ggplot(dplyr::tibble(x = test_dat$coord_input[,1],
                       y = test_dat$coord_input[,2],
                       sdf = test[,1]), aes(x, y)) +
    geom_point(aes(colour = sdf)) +
    scale_color_gradient2() +
    #scale_colour_continuous(trans = "sqrt") +
    coord_equal() +
    theme_minimal()
  
  test2 <- cbind(Oz_squamate_phylo_train_dat$train$x$coord_input[
    Oz_squamate_phylo_train_dat$train$x$phylo_id_input[,1] == test_num,
    ], 
    Oz_squamate_phylo_train_dat$train$y[
      Oz_squamate_phylo_train_dat$train$x$phylo_id_input[,1] == test_num
    ])
  
  ggplot(dplyr::tibble(x = test2[,1],
                       y = test2[,2],
                       sdf = test2[,3]), aes(x, y)) +
    geom_point(aes(colour = sdf)) +
    scale_color_gradient2() +
    coord_equal() +
    theme_minimal()

 
  plot(Oz)
  plot(Oz_squamates$geometry[Oz_squamates$Binomial == Oz_squamate_phylo_ids$species[test_num]],
       add = TRUE, col = "grey")

}
