##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Oz_squamate_phylo_x
##' @param Oz_squamate_sdf_train_val
make_Oz_squamate_train_dat <- function(Oz_squamate_sdf_train_val,
                                       Oz_squamate_phylo_ids) {

  train_dat <- Oz_squamate_sdf_train_val$train %>%
    dplyr::left_join(Oz_squamate_phylo_ids) %>%
    dplyr::select(-species) %>%
    dplyr::sample_frac()
  
  train_dat <- list(x = list(coord_input = train_dat %>%
                               dplyr::select(X, Y) %>%
                               as.matrix(),
                             phylo_id_input = train_dat %>%
                               dplyr::select(-X, -Y, -sdf) %>%
                               as.matrix()),
                    y = train_dat %>%
                      dplyr::select(sdf) %>%
                      as.matrix())
  
  
  val_dat <- Oz_squamate_sdf_train_val$validation %>%
    dplyr::left_join(Oz_squamate_phylo_ids) %>%
    dplyr::select(-species) %>%
    dplyr::sample_frac()
  
  val_dat <- list(x = list(coord_input = val_dat %>%
                               dplyr::select(X, Y) %>%
                               as.matrix(),
                             phylo_id_input = val_dat %>%
                               dplyr::select(-X, -Y, -sdf) %>%
                               as.matrix()),
                    y = val_dat %>%
                      dplyr::select(sdf) %>%
                      as.matrix())
  
  list(train = train_dat, validation = val_dat)

}
