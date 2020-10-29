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
library(shiny)
library(shinyalert)

path <- "./testing_dataset/"
image <- "181079"
test_image <- read_in_test_image(path = path, image = "181079")

segmentation_results_regular <-
  run_segmentation(test_image, mode = "regular")

display_results(test_image, segmentation_results_regular)

segmentation_results_mixed <-
  run_segmentation(test_image, mode = "mixed")

display_results(test_image, segmentation_results_mixed)

quality <-
  check_quality(segmentation_results_regular,
                segmentation_results_mixed,
                test_image)

# misclassified as object, as background
# overall accuracy
# accuracy inside and outside boarder area

ggplot(test_image) +
  facet_wrap(bs_neighborhood_tagging~., scales = "free_y") +
  geom_histogram(aes(dnorm_object))

ggplot(test_image) +
  facet_wrap(bs_neighborhood_tagging~., scales = "free_y") +
  geom_histogram(aes(dnorm_background))



set.seed(17)
image_list <- list.files(path = "testing_dataset/images/", pattern = "bmp")
image_list <- gsub(".bmp", "", image_list)
for (image in image_list) {
  print(paste("image: ", image))
  path <- "./testing_dataset/"
  test_image <- read_in_test_image(path = path, image = image)

  segmentation_results_regular <-
    run_segmentation(test_image, mode = "regular")

  segmentation_results_mixed <- tryCatch(
    {
      run_segmentation(test_image, mode = "mixed")
    },
    error=function(cond) {
      message("An error occured: ")
      message(cond)
      message("222")
      # Choose a return value in case of error
      return(NA)
    }
  )
  check_quality(segmentation_results_regular,
                segmentation_results_mixed,
                test_image)

}


results <- data.table::fread(file = "quality_results/metrics.csv") %>% na.omit()
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
