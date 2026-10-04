dat <- readr::read_csv(
  "data/cleaned_questionnaire_data.csv",
  show_col_types = FALSE
)

item_names <- grep("^q\\d{2}$", names(dat), value = TRUE)
items <- as.data.frame(dat[, item_names])

item_descriptives <- lapply(
  item_names,
  function(item) {
    x <- as.numeric(dat[[item]])
    other_items <- setdiff(item_names, item)

    total_without_item <- rowMeans(
      dat[, other_items],
      na.rm = TRUE
    )

    corrected_item_total_r <- suppressWarnings(
      stats::cor(
        x,
        total_without_item,
        use = "pairwise.complete.obs"
      )
    )

    data.frame(
      item = item,
      n_observed = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      iqr = stats::IQR(x, na.rm = TRUE),
      missing_percent = 100 * mean(is.na(x)),
      floor_percent = 100 * mean(x == 1, na.rm = TRUE),
      ceiling_percent = 100 * mean(x == 5, na.rm = TRUE),
      corrected_item_total_r = corrected_item_total_r
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

readr::write_csv(
  item_descriptives,
  "output/item_descriptives.csv"
)

message("Item-level descriptive statistics complete.")
