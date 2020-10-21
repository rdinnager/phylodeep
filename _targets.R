library(targets)
library(conflicted)

# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr",
                            "ape",
                            "sf",
                            "readr"))

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
               filter(Binomial %in% squamate_names$name)),
  
  tar_target(squamate_trees, read.tree(squamate_tree_file)),
  
  tar_target(squamate_tree, 
             squamate_trees[[sample.int(length(squamate_trees), 
                                        1L)]]),
  
  tar_target(Oz_squamate_tree, 
             drop.tip(squamate_tree,
                      which(!squamate_tree$tip.label %in%
                              gsub(" ", "_",
                                   Oz_squamates$Binomial))))
  
)

# End with a call to tar_pipeline() to wrangle the targets together.
# This target script must return a pipeline object.
tar_pipeline(targets)
