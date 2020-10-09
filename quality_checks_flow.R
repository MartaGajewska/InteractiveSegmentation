test_images <- read_in_test_images()
test_brush_strokes <- read_in_test_brush_strokes()
test_ground_truth <- read_in_test_ground_truth()

segmentation_results_regular <-
  run_segmentation_regular(test_images, test_brush_strokes)
segmentation_results_improved <-
  run_segmentation_improved(test_images, test_brush_strokes)

quality <-
  check_quality(segmentation_results_regular,
                segmentation_results_improved,
                test_ground_truth)
