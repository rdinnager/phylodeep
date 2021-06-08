##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param Oz_squamate_tree
calc_Oz_squamate_phylo_x <- function(Oz_squamate_tree, Oz_squamate_phylo_ids) {

  tip_mat <- makeL(Oz_squamate_tree)
  max_path <- max(tip_mat[ , -1])
  tip_mat[ , -1] <- tip_mat[ , -1] / max_path 
  colnames(tip_mat)[1] <- "root"
  
  phylo_df <- as.data.frame(tip_mat) %>%
    dplyr::mutate(species = rownames(tip_mat)) %>%
    dplyr::left_join(Oz_squamate_phylo_ids) %>%
    dplyr::select(species, phylo_id, dplyr::everything()) %>%
    dplyr::mutate(dplyr::across(c(-species, -phylo_id, -root), .fns = ~ (.x - mean(unique(.x))) * 2) + rnorm(dplyr::n(), 0, 0.001))
  
  phylo_df

}
