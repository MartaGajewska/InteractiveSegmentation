read_in_test_image <- function(path, image){
  test_base_image <- read_in_test_base_image(paste0(path, "images/"), image)
  test_brush_strokes <- read_in_test_brush_strokes(paste0(path, "brush_strokes/"), image)
  test_ground_truth <- read_in_test_ground_truth(paste0(path, "ground_truth/"), image)

  test_image <- test_base_image %>%
    left_join(test_brush_strokes %>%
                select(node_id, bs_tagging, bs_neighborhood_tagging),
              by = "node_id") %>%
    left_join(test_ground_truth %>%
                select(node_id, is_object_truth),
              by = "node_id")

  return(test_image)
}

read_in_test_base_image <- function(path, image){
  path = "./testing_dataset/images/"
  get_image(paste0(path, image, ".bmp")) %>% conv_image_to_df
}

read_in_test_brush_strokes <- function(path, image){
  path = "./testing_dataset/brush_strokes//"

  bs_df <- get_image(paste0(path, image, "-anno.png")) %>%
    conv_image_to_df %>%
    mutate(bs_tagging = ifelse(rgb_value == "#FFFFCF", "object",
                       ifelse(rgb_value == "#DB0000", "background",
                              NA)))

  bs_simplified_dt <-
    bs_df %>%
    select(column, row, node_id, bs_tagging) %>%
    as.data.table()

  bs_tagged_dt <- bs_simplified_dt[!is.na(bs_tagging),]

  tagged_neighborhood_dt <- data.table::rbindlist(list(
    copy(bs_tagged_dt)[, `:=`(mod_col = 1, mod_row = 0)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -1, mod_row = 0)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 0, mod_row = 1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 0, mod_row = -1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 2, mod_row = 0)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -2, mod_row = 0)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 0, mod_row = 2)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 0, mod_row = -2)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 1, mod_row = 1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -1, mod_row = 1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 1, mod_row = -1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -1, mod_row = -1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 1, mod_row = 2)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -1, mod_row = 2)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 1, mod_row = -2)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -1, mod_row = -2)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 2, mod_row = 1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -2, mod_row = 1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = 2, mod_row = -1)],
    copy(bs_tagged_dt)[, `:=`(mod_col = -2, mod_row = -1)]
  )
  )
  tagged_neighborhood_dt[, `:=`(ngb_column = column + mod_col, ngb_row = row + mod_row)]

  tagged_neighborhood_dt <-
    merge(
      tagged_neighborhood_dt,
      copy(bs_simplified_dt),
      by.x = c('ngb_column', 'ngb_row'),
      by.y = c('column', 'row'),
      all.x = TRUE
    )[!is.na(node_id.y),][is.na(bs_tagging.y),][,c("node_id.y", "bs_tagging.x")]

  colnames(tagged_neighborhood_dt) <- c("node_id", "bs_neighborhood_tagging")

  bs_df <- bs_df %>%
    left_join(tagged_neighborhood_dt, by = "node_id") %>%
    mutate(bs_neighborhood_tagging =
             ifelse(is.na(bs_tagging), bs_neighborhood_tagging, bs_tagging)) %>%
    distinct(node_id, .keep_all = TRUE)

  return(bs_df)
}

read_in_test_ground_truth <- function(path, image){
  path = "./testing_dataset/ground_truth/"
  get_image(paste0(path, image, ".png")) %>%
    conv_image_to_df %>%
    mutate(is_object_truth = ifelse(node_value_cc_1 > 0.5, TRUE, FALSE))
}

get_image <- function(image_path) {
  # Max 50 x 100 pixels
  # 100 x 200 was slow (few minutes)
  image <- load.image(image_path)
  # setting 80 to be the max size, keeping the aspect ratio
  dim <- image %>% dim %>% .[1:2]
  new_dim <- round(80*dim/max(dim))

  image <- image %>%
    resize(new_dim[1], new_dim[2])

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
