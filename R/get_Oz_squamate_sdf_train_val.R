##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Oz_squamate_sdf_samples
get_Oz_squamate_sdf_train_val <- function(Oz_squamate_sdf_samples) {

  train_val <- Oz_squamate_sdf_samples %>%
    dplyr::group_by(species)
  
  specs <- dplyr::group_keys(train_val)
  
  train_val <- train_val %>%
    dplyr::group_map(~make_cross_validations(.x,
                                            validation_type = "random",
                                            validation_folds = 1,
                                            validation_split = 10,
                                            validation_prop = 1 / 20,
                                            use_coords = TRUE)[[1]])
  
  train_val <- purrr::transpose(train_val)
  
  train_val$train <- purrr::map2_dfr(train_val$train, specs$species,
                                     ~dplyr::tibble(X = .x$x[ , 1],
                                                    Y = .x$x[ , 2],
                                                    sdf = .x$y) %>%
                                       dplyr::mutate(species = .y))
  
  train_val$validation <- purrr::map2_dfr(train_val$validation, specs$species,
                                     ~dplyr::tibble(X = .x$x[ , 1],
                                                    Y = .x$x[ , 2],
                                                    sdf = .x$y) %>%
                                       dplyr::mutate(species = .y))
  
  names(train_val$intervals) <- specs
  
  train_val

}
