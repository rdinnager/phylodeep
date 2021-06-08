##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Oz_squamates
##' @param Oz
get_Oz_squamate_sdf_samples <- function(Oz_squamates, Oz) {

  shape_centroid <- sf::st_centroid(Oz %>% sf::st_union())
  st <- Oz - shape_centroid
  norms <- st %>%
    sf::st_coordinates() %>%
    .[ , 1:2] %>%
    apply(1, function(x) sqrt(sum(x^2)))
  
  scaler <- max(norms)
  
  centrer <- shape_centroid %>% sf::st_coordinates() %>%
    as.vector()
  
  plan(multisession(workers = 6))
  
  squamate_sdfs <- furrr::future_map_dfr(seq_len(nrow(Oz_squamates)),
                                         purrr::possibly(~rngnet::collect_sdf_samples(Oz_squamates$geometry[.x],
                                                                      Oz, centrer = centrer,
                                                                      scaler = scaler,
                                                                      drop_NAs = TRUE,
                                                                      n_pts = 25000,
                                                                      equal_in_out = TRUE) %>%
                                           mutate(species = Oz_squamates$Binomial[.x]),
                                           otherwise = dplyr::tibble()),
                                         .progress = TRUE)
  
  # purrr::map_dfr(seq_len(nrow(Oz_squamates)),
  #                       purrr::possibly(~{rngnet::collect_sdf_samples(Oz_squamates$geometry[.x],
  #                                                     Oz, centrer = centrer,
  #                                                     scaler = scaler,
  #                                                     drop_NAs = TRUE,
  #                                                     n_pts = 25000,
  #                                                     equal_in_out = TRUE) %>%
  #                           mutate(species = Oz_squamates$Binomial[.x]);
  #                         print(.x)},
  #                         otherwise = dplyr::tibble()))
  # 
  # 
  # test <- rngnet::collect_sdf_samples(Oz_squamates$geometry[7],
  #                                     Oz, centrer = centrer,
  #                                     scaler = scaler,
  #                                     drop_NAs = TRUE,
  #                                     n_pts = 25000) %>%
  #   mutate(species = Oz_squamates$Binomial[1])
  
  squamate_sdfs

}
