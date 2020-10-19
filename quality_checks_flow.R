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
