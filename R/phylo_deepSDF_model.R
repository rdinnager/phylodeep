
deepSDF_phylo_model <- function(input_len = 2L,
                                phylo_mat,
                                latent_dim = 32L,
                                net_breadth = 256L, 
                                dropout_rate = 0.5,
                                ridge_lambda = 0.01) {
  
  coord_input <- keras::layer_input(shape = c(input_len), dtype = "float32", name = "coord_input")
  phylo_id_input <- keras::layer_input(shape = c(1), dtype = "int32", name = "phylo_id_input")
  
  phylo_tens <- keras::keras_array(phylo_mat)
  
  phylo_output <- phylo_tens %>%
    keras::k_gather(keras::k_squeeze(phylo_id_input, 0L) - 1L) %>%
    keras::layer_dense(units = latent_dim,
                       kernel_regularizer = keras::regularizer_l2(ridge_lambda))
  
  code_concat <- keras::layer_concatenate(list(coord_input, phylo_output))
  
  first_block <- code_concat %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate)
  
  sdf_output <- keras::layer_concatenate(list(first_block, code_concat)) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = net_breadth, use_bias = FALSE) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activation_leaky_relu() %>%
    keras::layer_dropout(rate = dropout_rate) %>%
    keras::layer_dense(units = 1L, activation = "tanh")
  
  model <- keras::keras_model(
    inputs = list(coord_input, phylo_id_input),
    outputs = sdf_output
  )
  
  model
}

SDF_loss_mae <- function(sdf_cutoff = 0.1) {
  
  sdf_cutoff <- sdf_cutoff
  
  function(y_true, y_pred) {
    
    keras::loss_mean_absolute_error(keras::k_clip(y_true, -sdf_cutoff, sdf_cutoff), 
                                    keras::k_clip(y_pred, -sdf_cutoff, sdf_cutoff))
    
  }
}