library(targets)
# This is an example target script.
# Read the tar_script() help file for details.

source("packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

# dryad_package_dois('10.5061/dryad.83s7k')
# dryad_files('10.5061/dryad.83s7k')

# Define targets
targets <- list(
  
  tar_target(squamate_name_file, 
             "data/squamates/sqamate_names.csv",
             format = "file"),
  
  tar_target(squamate_tree_file, 
             "data/squamates/trees/squam_shl_new_Posterior_9755.1000.trees",
             format = "file"),
  
  tar_target(rep_shp, 
             "data/squamates/ranges/modeled_reptiles.shp",
             format = "file"),
  
  tar_target(reps, 
             read_sf(rep_shp) %>%
               st_make_valid(),
             format = "qs"),
  
  tar_target(Oz, ne_countries(scale = 50, country = "Australia", returnclass = "sf") %>%
               ms_filter_islands(100000000) %>%
               st_union() %>%
               st_make_valid(),
    packages = c("dplyr", "sf", "rmapshaper", "rnaturalearth")),
  
  tar_target(Oz_reps, reps %>%
               st_intersection(Oz)),
  
  tar_target(squamate_names, read_csv(squamate_name_file,
                                      col_names = "name")),
  
  tar_target(Oz_squamates, Oz_reps %>%
               filter(Binomial %in% squamate_names$name) %>%
               mutate(Binomial = gsub(" ", "_", Binomial))),
  
  tar_target(squamate_trees, read.tree(squamate_tree_file)),
  
  tar_target(squamate_tree, 
             squamate_trees[[sample.int(length(squamate_trees), 
                                        1L)]]),
  
  tar_target(Oz_squamate_tree, 
             drop.tip(squamate_tree,
                      which(!squamate_tree$tip.label %in%
                              Oz_squamate_sdf_samples$species))),
  
  tar_target(Oz_squamate_phylo_ids, 
             dplyr::tibble(species = Oz_squamate_tree$tip.label,
                           phylo_id = 1:length(Oz_squamate_tree$tip.label))),
  
  tar_target(Oz_squamate_sdf_samples,
             get_Oz_squamate_sdf_samples(Oz_squamates,
                                         Oz),
             format = "fst_tbl"),
  
  tar_target(Oz_squamate_sdf_train_val,
             get_Oz_squamate_sdf_train_val(Oz_squamate_sdf_samples),
             format = "qs"),
  
  tar_target(Oz_squamate_phylo_x,
             calc_Oz_squamate_phylo_x(Oz_squamate_tree,
                                      Oz_squamate_phylo_ids)),
  
  tar_target(Oz_squamate_phylo_train_dat,
             make_Oz_squamate_train_dat(Oz_squamate_sdf_train_val,
                                        Oz_squamate_phylo_ids),
             format = "qs"),
  
  tar_target(Oz_squamate_trained_model,
             train_Oz_squamate_model(Oz_squamate_phylo_train_dat,
                                     Oz_squamate_phylo_x,
                                     save_file = "trained_models/Oz_squamate_ranges_phylo.hd5")),
  
  tar_target(Oz_squamate_trained_model_reload,
             list(model_weights = Oz_squamate_trained_model,
                  required_data = Oz_squamate_phylo_x)),
  
  tar_target(Oz_prediction_grid, rngnet::make_prediction_grid(NULL,
                                                              bg = Oz,
                                                              use_coords = TRUE)),
  
  tar_target(Oz_squamate_compare_preds,
             compare_Oz_squamate_predictions(Oz_squamate_trained_model_reload,
                                             Oz_squamate_phylo_ids,
                                             Oz_prediction_grid,
                                             Oz_squamates)),
  
  NULL
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
