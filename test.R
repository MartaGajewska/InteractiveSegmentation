# image_with_node_values <- calc_node_values_alternative(image_df, limits_object, limits_background)

calc_params_test <- function(image_df, limits){

  # TODO: doesn't work for a singular martix
  selected_pixels <- image_df %>%
    filter(between(column, limits$xmin, limits$xmax) &
             between(row, limits$ymin, limits$ymax)) %>%
    select(node_value_cc_1, node_value_cc_2, node_value_cc_3)

  mixmdl <- mixtools::mvnormalmixEM(selected_pixels %>% as.data.frame(),
                                    k = 3)

  return(list(means = mixmdl$mu, vars = mixmdl$sigma, lambdas = mixmdl$lambda))
}


calc_node_values_alternative <- function(image_df, limits_object, limits_background){

  neigh_coef <- 2

  # Display image with selected areas
  plot_selections(image_df, limits_object, limits_background) %>% plot

  # Get parameters from selections
  object_params <- calc_params_test(image_df, limits_object)
  background_params <- calc_params_test(image_df, limits_background)

  # Define graph vertices and capacities

  # Calculate probabilities
  # if(sum(object_params$var!=object_params$var[1,1])==0){
  #   print("ERROR - this case is not handled yet!")
  #   # All color channels are identical
  #   image_df <- image_df %>%
  #     mutate(dnorm_object = dnorm(node_value_cc_1,
  #                                 mean = object_params$means[1],
  #                                 sd = object_params$var[1,1]),
  #            dnorm_background = dnorm(node_value_cc_1,
  #                                     mean = background_params$means[1],
  #                                     sd = background_params$var[1,1]))
  # } else {
    # Different color channels
  # browser()
    image_df <- image_df %>%
      mutate(dnorm_object =
               purrr::map_dfc(1:length(object_params$lambdas),
                              function(i) object_params$lambda[i] * mvtnorm::dmvnorm(
                                image_df %>% select(starts_with("node_value_")),
                                mean = object_params$means[[i]],
                                sigma = object_params$vars[[i]])) %>% rowSums(),
             dnorm_background =
               purrr::map_dfc(1:length(background_params$lambdas),
                          function(i) background_params$lambda[i] * mvtnorm::dmvnorm(
                            image_df %>% select(starts_with("node_value_")),
                            mean = background_params$means[[i]],
                            sigma = background_params$vars[[i]])) %>% rowSums())

  image_df <- image_df %>%
    mutate(is_background =
             between(column, limits_background$xmin, limits_background$xmax) &
             between(row, limits_background$ymin, limits_background$ymax)) %>%
    mutate(is_object =
             between(column, limits_object$xmin, limits_object$xmax) &
             between(row, limits_object$ymin, limits_object$ymax))

  ## Source (object) - pixels
  source_ver <- image_df %>%
    rename(`to` = `node_id`) %>%
    # mutate(capacity = -log(dnorm_object)) %>%
    mutate(capacity = (dnorm_object)) %>%
    # add some extra to the selected pixels to keep user constrain
    mutate(capacity = ifelse(is_object, 1000000000000, capacity)) %>%
    mutate(from = 1) %>%
    select(from, to, capacity)

  ## Pixels - sink
  sink_ver <- image_df %>%
    rename(from=node_id) %>%
    # mutate(capacity = -log(dnorm_background)) %>%
    mutate(capacity = (dnorm_background)) %>%
    # add some extra to the selected pixels to keep user constrain
    mutate(capacity = ifelse(is_background, 1000000000000, capacity)) %>%
    mutate(to = 2) %>%
    select(from, to, capacity)

  ## Pixels - pixels (smoothness term)

  neighborhood_ver <- calc_cap_neighborhood_ver(image_df, neigh_coef)

  # Combine source, sink and neighbouring vertices into a graph
  image_with_node_values <- rbindlist(list(source_ver, sink_ver, neighborhood_ver))

  return(image_with_node_values)
}

# mvtnorm::dmvnorm(image_df %>% select(starts_with("node_value_")),
#                  mean  = object_params$means,
#                  sigma = object_params$var)
