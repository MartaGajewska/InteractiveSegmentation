# library(imager)
# library(igraph)
# library(dplyr)
# library(data.table)
# library(ggplot2)
# library(patchwork)
# library(shiny)
# library(shinyalert)

source("functions.R")
source("app.R")
# GRAPH CUT

# Prepare image
image <- get_image("sun.jpg")
# image <- get_image("bird.jpg")
# image <- get_image("Picture.PNG")
# image <- get_image("sun.jpg")

image_df <- conv_image_to_df(image)


# Get user input on background and object
# shinyApp(ui, server)

# Create a graph with vertices and their capacities
limits_object <- readRDS(list.files(".",pattern="object"))
limits_background <- readRDS(list.files(".",pattern="background"))
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

# the simplest way to compute P_F and P_B is to first fit a couple of
# Gaussian distributions on the scribbles by computing the parameters (μ, ∑)
# with MLE from the scribbled pixel intensities and then computing the (class-conditional)
# probabilities from the individual pdfs (followed by a normalization)
# for each of the pixels as shown in the next figures.
# The following code fragment show how the pdfs are being computed.
