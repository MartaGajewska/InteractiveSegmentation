run_segmentation <- function(test_image, mode){
  # Display image with selected areas
  plot_selections(test_image) %>% plot

  image_with_node_values <- calc_node_values(test_image, mode)
  image_graph <- conv_image_to_graph(image_with_node_values)

  # Create image partitioning using max flow algorithm
  partitioning <-
    igraph::max_flow(
      image_graph,
      source = igraph::V(image_graph)["1"],
      target = igraph::V(image_graph)["2"]
    )

  return(partitioning)
}

plot_selections <- function(test_image){

  ggplot(test_image, aes(column, row)) +
    geom_raster(aes(fill=rgb_value))  +
    scale_y_reverse() +
    scale_fill_identity() +
    theme(legend.position="none") +
    theme(legend.position="none") +
    geom_point(data = test_image %>% filter(bs_neighborhood_tagging == "object"),
               aes(column, row),
               color = "blue") +
    geom_text(
      aes(x = test_image %>% filter(bs_neighborhood_tagging == "object") %>% pull(column) %>% min,
          y = test_image %>% filter(bs_neighborhood_tagging == "object") %>% pull(row) %>% min,
          label = "object"),
      size = 3, vjust = -0.3, hjust = -0.1, color = "blue"
    ) +
    geom_point(data = test_image %>% filter(bs_neighborhood_tagging == "background"),
               aes(column, row),
               color = "red") +
    geom_text(
      aes(x = test_image %>% filter(bs_neighborhood_tagging == "background") %>% pull(column) %>% min,
          y = test_image %>% filter(bs_neighborhood_tagging == "background") %>% pull(row) %>% min,
          label = "background"),
      size = 3, vjust = -0.3, hjust = -0.1, color = "red"
    ) +
    coord_fixed(ratio=1) +
    theme_void()
}

calculate_capacites <- function(test_image, object_params, background_params, mode){

  if (mode == "regular") {
  if(sum(object_params$var!=object_params$var[1,1])==0){
    # All color channels are identical
    test_image <- test_image %>%
      mutate(dnorm_object = dnorm(node_value_cc_1,
                                  mean = object_params$means[1],
                                  sd = object_params$var[1,1]),
             dnorm_background = dnorm(node_value_cc_1,
                                      mean = background_params$means[1],
                                      sd = background_params$var[1,1]))
  } else {
    # Different color channels
    test_image <- test_image %>%
      mutate(dnorm_object = mvtnorm::dmvnorm(test_image %>% select(starts_with("node_value_")),
                                             mean  = object_params$means,
                                             sigma = object_params$var),
             dnorm_background = mvtnorm::dmvnorm(test_image %>% select(starts_with("node_value_")),
                                                 mean  = background_params$means,
                                                 sigma = background_params$var))
  }
  } else {

    test_image <- test_image %>%
      mutate(dnorm_object =
               purrr::map_dfc(1:length(object_params$lambdas),
                              function(i) object_params$lambda[i] * mvtnorm::dmvnorm(
                                test_image %>% select(starts_with("node_value_")),
                                mean = object_params$means[[i]],
                                sigma = object_params$vars[[i]])) %>% rowSums(),
             dnorm_background =
               purrr::map_dfc(1:length(background_params$lambdas),
                              function(i) background_params$lambda[i] * mvtnorm::dmvnorm(
                                test_image %>% select(starts_with("node_value_")),
                                mean = background_params$means[[i]],
                                sigma = background_params$vars[[i]])) %>% rowSums())
  }

  return(test_image)
}

calc_node_values <- function(test_image, mode){

  neigh_coef <- 5

  # Get parameters from selections
  object_params <- calc_params(test_image, "object", mode)
  background_params <- calc_params(test_image, "background", mode)

  # Define graph vertices and capacities

  # Calculate probabilities
  test_image <- calculate_capacites(test_image, object_params, background_params, mode)

  ## Source (object) - pixels
  source_ver <- test_image %>%
    rename(`to` = `node_id`) %>%
    # mutate(capacity = -log(dnorm_object)) %>%
    mutate(capacity = dnorm_object) %>%
    # add some extra to the selected pixels to keep user constrain
    mutate(capacity = ifelse(is.na(bs_neighborhood_tagging) | bs_neighborhood_tagging != "object", capacity, 1000000000000)) %>%
    mutate(from = 1) %>%
    select(from, to, capacity)

  ## Pixels - sink
  sink_ver <- test_image %>%
    rename(from=node_id) %>%
    # mutate(capacity = -log(dnorm_background)) %>%
    mutate(capacity = dnorm_background) %>%
    # add some extra to the selected pixels to keep user constrain
    mutate(capacity = ifelse(is.na(bs_neighborhood_tagging) | bs_neighborhood_tagging != "backgroud", capacity, 1000000000000)) %>%
    mutate(to = 2) %>%
    select(from, to, capacity)

  ## Pixels - pixels (smoothness term)

  neighborhood_ver <- calc_cap_neighborhood_ver(test_image, neigh_coef)

  # Combine source, sink and neighbouring vertices into a graph
  image_with_node_values <- rbindlist(list(source_ver, sink_ver, neighborhood_ver))

  return(image_with_node_values)
}

calc_cap_neighborhood_ver <- function(image_df, neigh_coef) {
  # Create a df with neighbouring (4) pixels
  # Each pair is in the subset twice (x -> y & y -> x)
  image_dt <- data.table(image_df)

  neighborhood_dt <- data.table::rbindlist(list(
    copy(image_dt)[,`:=`(mod_col= 1, mod_row = 0)],
    copy(image_dt)[,`:=`(mod_col = -1, mod_row = 0)],
    copy(image_dt)[,`:=`(mod_col = 0, mod_row = 1)],
    copy(image_dt)[,`:=`(mod_col = 0, mod_row = -1)]
  )
  )

  # calculate neighbor coordinates
  neighborhood_dt[, `:=`(ngb_column = column + mod_col, ngb_row = row + mod_row)]

  # find neighbor pixel value
  total_smoothness <- 2 # 5 is recommended
  similarity_smoothness <- 50/10 # 50 is recommended
  contrast <- 3
  # MASS::fitdistr(image_df$node_value, "normal")$estimate[['sd']]^2

  neighborhood_ver <-
    merge(
      neighborhood_dt,
      copy(image_dt),
      by.x = c('ngb_column', 'ngb_row'),
      by.y = c('column', 'row'),
      all.x = TRUE
    )[!is.na(node_id.y),][
      , capacity := (total_smoothness + similarity_smoothness*exp(-(
        (node_value_cc_1.x - node_value_cc_1.y)^2 +
          (node_value_cc_2.x - node_value_cc_2.y)^2 +
          (node_value_cc_3.x - node_value_cc_3.y)^2
      ))*contrast)*neigh_coef
      ][,c("node_id.x", "node_id.y", "capacity")]

  setnames(neighborhood_ver,
           colnames(neighborhood_ver),
           c("from", "to", "capacity"))

  return(neighborhood_ver)
}

calc_params <- function(test_image, type, mode) {
  selected_pixels <- test_image %>%
    filter(bs_neighborhood_tagging == type) %>%
    select(node_value_cc_1, node_value_cc_2, node_value_cc_3)

  if (mode == "regular") {

    means <- colMeans(selected_pixels)
    var <- var(selected_pixels)

    output <- list(means = means, var = var)
  } else {

    mixmdl <-
      mixtools::mvnormalmixEM(selected_pixels %>% as.data.frame(), k = 3)

    output <- list(means = mixmdl$mu, vars = mixmdl$sigma, lambdas = mixmdl$lambda)
  }

  return(output)
}

conv_image_to_graph <- function(image_with_node_values){

  image_graph <- igraph::graph_from_data_frame(as.data.frame(image_with_node_values))

  return(image_graph)
}


display_results <- function(image_df, partitioning) {
# TODO: show a triple here
  origin <-
    ggplot(image_df, aes(column, row)) +
    geom_raster(aes(fill=rgb_value), hjust = 0.5)  +
    scale_y_reverse() +
    scale_fill_identity() +
    lims(x = c(min(image_df$column), max(image_df$column))) +
    theme(legend.position="none")

  # TODO: Figure out why we need a shift here
  output <-
    ggplot() +
    geom_raster(data = image_df %>%
                  filter(column != 1 ),
                aes(column-1, row, fill=rgb_value), hjust = 0.5)  +
    geom_point(data = image_df %>%
                 filter(node_id %in% partitioning$partition2) %>%
                 filter(column != max(image_df$column)),
               aes(column, row), color = "red", alpha = 0.2) +
    scale_y_reverse() +
    scale_fill_identity() +
    theme(legend.position="none")

  return(origin|output)
}
