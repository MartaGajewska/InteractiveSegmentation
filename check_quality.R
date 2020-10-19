check_quality <- function(segmentation_results_regular,
                          segmentation_results_mixed,
                          test_image) {
  quality_regular <- calc_quality_metrics(segmentation_results_regular,
                                          test_image)
  quality_mixed <- calc_quality_metrics(segmentation_results_mixed,
                                        test_image)

  # Generate report
  # Individual level:
  # 3 input pictures, 2 output pictures, a table with metrics
  # generate_report(test_image, quality_regular, quality_mixed)

  # Save metrics to csv file
  # save quality_regular, quality_mixed, append = TRUE

  # TODO: add error handling before - singular matrix - NAs, etc
}

calc_quality_metrics <- function(segmentation_results,
                                 test_image) {
  # misclassified as object, as background
  # overall accuracy
  # accuracy inside and outside boarder area
  test_image <- test_image %>%
    mutate(marked_as_object =
             ifelse(node_id %in% segmentation_results_mixed$partition1, TRUE, FALSE))

  conf_mat <- caret::confusionMatrix(data = as.factor(test_image$marked_as_object),
                         reference = as.factor(test_image$is_object_truth))

  # Precision - how many of the pixels classified as object are actually object
  # (out of all pixels marked as O, how many are really O - how much have you misclasified as object)
  # Recall/Sensitivity - how many of actual object pixels were classified as object
  # (out of all O pixels, how many are marked as O - how much have you found)
  # Specificity - how many of B pixels were marked as B
  # (how many background pixels you were able to find)
  quality_metrics <- c(conf_mat$overall["Accuracy"],
    conf_mat$byClass[c("Precision", "Sensitivity", "Specificity", "Balanced Accuracy")])

  return(quality_metrics)
}
