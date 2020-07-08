library(imager)
library(igraph)
library(dplyr)
library(data.table)
library(ggplot2)
library(Cairo)
library(patchwork)
library(shiny)
library(shinyalert)

source("functions.R")
source("app.R")
# GRAPH CUT

# Prepare image
image <- get_image("image_examples/sun.jpg")
# image <- get_image("image_examples/bird.jpg")

image_df <- conv_image_to_df(image)


# Get user input on background and object
shinyApp(ui, server)

# Create a graph with vertices and their capacities
limits_object <- readRDS(paste0("./app_data", list.files("./app_data",pattern="object")))
limits_background <- readRDS(paste0("./app_data", list.files("./app_data",pattern="background")))
image_with_node_values <- calc_node_values(image_df, limits_object, limits_background)
image_graph <- conv_image_to_graph(image_with_node_values)


partitioning <-
  igraph::max_flow(
    image_graph,
    source = igraph::V(image_graph)["1"],
    target = igraph::V(image_graph)["2"]
  )

# Check results
# partitioning$partition1
# partitioning$partition2
display_results(image_df, partitioning)

