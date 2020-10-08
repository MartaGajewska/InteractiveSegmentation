get_image <- function(image_path) {
  # Max 50 x 100 pixels
  # 100 x 200 was slow (few minutes)
  image <- load.image(image_path)

  image <- image %>%
    resize(min(50, width(image)),
           min(50, height(image)))

  # Convert to grayscale and perform histogram normalization
  # image <- grayscale(image)
  # noise <- 0.2*imnoise(dim=dim(image)) %>% isoblur(2)
  # image <- image + noise

  plot(image)

  return(image)
}

conv_image_to_df <- function(image){

  # convert to graph
  image_df <-
    image %>%
    as.data.frame()

  if (!("cc" %in% colnames(image_df))) {
    image_df <- image_df %>%
      merge(data.frame(cc = c(1, 2, 3)))
  }

  image_df <-
    image_df %>%
    tidyr::pivot_wider(names_from = cc, names_prefix = "node_value_cc_") %>%
    mutate(rgb_value = rgb(red = node_value_cc_1, green = node_value_cc_2, blue = node_value_cc_3)) %>%
    rename(column = x,
           row = y
    ) %>%
    # Node 1 = source, node 2 = sink
    mutate(node_id = 2+row_number())

  # wide histogram
  # image_df$node_value <- (image_df$node_value - min(image_df$node_value))/(max(image_df$node_value)-min(image_df$node_value))
  # image_df %>%
  #   group_by(cc) %>%
  #   mutate(node_value  =
  #            (node_value - min(node_value))/(max(node_value)-min(node_value))) %>%
  #   glimpse

  return(image_df)
}

calc_params <- function(image_df, limits){

  selected_pixels <- image_df %>%
    filter(between(column, limits$xmin, limits$xmax) &
             between(row, limits$ymin, limits$ymax)) %>%
    select(node_value_cc_1, node_value_cc_2, node_value_cc_3)

  means <- colMeans(selected_pixels)

  var <- var(selected_pixels)

  return(list(means = means, var = var))
}

plot_selections <- function(image_df, limits_object, limits_background){

  ggplot(image_df, aes(column, row)) +
    geom_raster(aes(fill=rgb_value))  +
    scale_y_reverse() +
    scale_fill_identity() +
    theme(legend.position="none") +
    # lims(x = c(min(image_df$column), max(image_df$column))) +
    theme(legend.position="none") +
    geom_rect(aes(xmin = limits_object$xmin, xmax = limits_object$xmax,
                  ymin = limits_object$ymin, ymax = limits_object$ymax),
              fill = "transparent", color = "red", size = 1) +
    geom_text(
      aes(x = limits_object$xmin, y = limits_object$ymax, label = "object"),
      size = 3, vjust = -0.3, hjust = -0.1, color = "red"
    ) +
    geom_rect(aes(xmin = limits_background$xmin, xmax = limits_background$xmax,
                  ymin = limits_background$ymin, ymax = limits_background$ymax),
              fill = "transparent", color = "blue", size = 1) +
    geom_text(
      aes(x = limits_background$xmin, y = limits_background$ymax, label = "background"),
      size = 3, vjust = -0.3, hjust = -0.1, color = "blue"
    ) +
    coord_fixed(ratio=1)

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
      ] [
        ,c("node_id.x", "node_id.y", "capacity")]

  setnames(neighborhood_ver,
           colnames(neighborhood_ver),
           c("from", "to", "capacity"))

  return(neighborhood_ver)
}


calc_node_values <- function(image_df, limits_object, limits_background){

  neigh_coef <- 2

  # Display image with selected areas
  plot_selections(image_df, limits_object, limits_background) %>% plot

  # Get parameters from selections
  object_params <- calc_params(image_df, limits_object)
  background_params <- calc_params(image_df, limits_background)

  # Define graph vertices and capacities

  # Calculate probabilities
  if(sum(object_params$var!=object_params$var[1,1])==0){
    # All color channels are identical
    image_df <- image_df %>%
      mutate(dnorm_object = dnorm(node_value_cc_1,
                                  mean = object_params$means[1],
                                  sd = object_params$var[1,1]),
             dnorm_background = dnorm(node_value_cc_1,
                                  mean = background_params$means[1],
                                  sd = background_params$var[1,1]))
  } else {
    # Different color channels
    image_df <- image_df %>%
      mutate(dnorm_object = mvtnorm::dmvnorm(image_df %>% select(starts_with("node_value_")),
                                                 mean  = object_params$means,
                                                 sigma = object_params$var),
             dnorm_background = mvtnorm::dmvnorm(image_df %>% select(starts_with("node_value_")),
                                                 mean  = background_params$means,
                                                 sigma = background_params$var))
  }


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

conv_image_to_graph <- function(image_with_node_values){

  image_graph <- igraph::graph_from_data_frame(as.data.frame(image_with_node_values))

  return(image_graph)
}


display_results <- function(image_df, partitioning) {

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
