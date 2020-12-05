source("read_in_test_set.R")
source("run_segmentation.R")
source("check_quality.R")
library(imager)
library(igraph)
library(dplyr)
library(data.table)
library(ggplot2)
library(Cairo)
library(patchwork)
# library(shiny)
# library(shinyalert)

# path <- "./testing_dataset/"
# image <- "181079"
# test_image <- read_in_test_image(path = path, image = "181079")
#
# segmentation_results_regular <-
#   run_segmentation(test_image, mode = "regular")
#
# display_results(test_image, segmentation_results_regular)
#
# segmentation_results_mixed <-
#   run_segmentation(test_image, mode = "mixed")
#
# display_results(test_image, segmentation_results_mixed)
#
# quality <-
#   check_quality(segmentation_results_regular,
#                 segmentation_results_mixed,
#                 test_image)
#
# # misclassified as object, as background
# # overall accuracy
# # accuracy inside and outside boarder area
#
# ggplot(test_image) +
#   facet_wrap(bs_neighborhood_tagging~., scales = "free_y") +
#   geom_histogram(aes(dnorm_object))
#
# ggplot(test_image) +
#   facet_wrap(bs_neighborhood_tagging~., scales = "free_y") +
#   geom_histogram(aes(dnorm_background))


# if (file.exists("quality_results/metrics.csv")) {
#   #Delete file if it exists
#   file.remove("quality_results/metrics.csv)
# }

image_format <- "jpg"
image_list <- list.files(path = "testing_dataset/images/", pattern = image_format)
image_list <- gsub(paste0(".", image_format), "", image_list)
for (image in image_list) {
  set.seed(17)
  print(paste("image: ", image))
  path <- "./testing_dataset/"
  test_image <- read_in_test_image(path = path, image = image, image_format = image_format)

  segmentation_results_regular <-
    run_segmentation(test_image, mode = "regular")

  segmentation_results_mixed <- tryCatch(
    {
      print("inside mixed method")
      run_segmentation(test_image, mode = "mixed")
    },
    error=function(cond) {
      message("An error occured: ")
      message(cond)
      message("222")
      # Choose a return value in case of error
      return(list(partitioning = NA, plots = list(selection=NA, results = NA)))
    }
  )

  quality_plot <- check_quality(segmentation_results_regular$partitioning,
                segmentation_results_mixed$partitioning,
                test_image)

  final_plots <- cowplot::plot_grid(
    ggplot() +
      annotate("text", x = 4, y = 25, size=8, label = paste0("Image: ", image)) +
      theme_void() + theme(aspect.ratio=1/10),
    cowplot::plot_grid(
            segmentation_results_regular$plots$selection,
            segmentation_results_regular$plots$results,
            segmentation_results_mixed$plots$results,
            ncol = 3),
    quality_plot, ncol = 1, rel_heights = c(1,3,3))

  pdf(paste0("quality_results/image_reports/", image, ".pdf"), width = 14, height = 8)
  final_plots %>% plot()
  dev.off()

}

results <- data.table::fread(file = "quality_results/metrics.csv") %>% na.omit()

plots_order <- results %>% filter(metric == "Accuracy") %>%
  mutate(quality_diff = quality_mixed - quality_regular) %>%
  arrange(quality_diff) %>%
  pull(image) %>%
  unique()

pdftools::pdf_combine(paste0("quality_results/image_reports/", plots_order, ".pdf"), output = "quality_results/joined.pdf")




results_long <- results %>%
  tidyr::pivot_longer(cols = c(quality_regular, quality_mixed), names_to = "method", names_prefix = "quality_")

ggplot(results_long, aes(value, method, color = method)) +
  facet_grid(metric ~ .) +
  geom_violin(aes(fill = method), alpha  = 0.5) +
  geom_jitter() +
  geom_boxplot(fill =NA, color = "black") +
  NULL

results <- results %>%
  mutate(diff = quality_mixed - quality_regular)
results %>% group_by(metric) %>% summarise_all(mean)
results %>% filter(metric == "Accuracy") %>% summary()
ggplot(results, aes(diff))+
  geom_histogram()

x <- results %>%  filter(metric == "Accuracy") %>% nrow
y <- results %>%  filter(metric == "Accuracy") %>% filter(diff >0) %>%  nrow
y/x
# 75% przypadkow - lepiej

results %>% filter(metric=="Accuracy") %>%  arrange(diff) %>% ggplot(aes(image %>% reorder(diff), diff))+
  geom_point()
