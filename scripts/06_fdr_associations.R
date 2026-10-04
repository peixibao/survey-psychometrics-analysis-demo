dat <- readr::read_csv(
  "data/cleaned_questionnaire_data.csv",
  show_col_types = FALSE
)

dat$sex <- factor(dat$sex)
item_names <- grep("^q\\d{2}$", names(dat), value = TRUE)

item_results <- lapply(
  item_names,
  function(item) {
    model_formula <- stats::as.formula(
      paste0(
        "weekly_takeout ~ scale(",
        item,
        ") + age + sex + bmi"
      )
    )

    fit <- stats::lm(
      model_formula,
      data = dat
    )

    tidy_fit <- broom::tidy(
      fit,
      conf.int = TRUE
    )

    exposure_row <- tidy_fit[
      grepl("^scale\\(", tidy_fit$term),
      ,
      drop = FALSE
    ]

    data.frame(
      item = item,
      beta = exposure_row$estimate,
      std_error = exposure_row$std.error,
      conf_low = exposure_row$conf.low,
      conf_high = exposure_row$conf.high,
      p_value = exposure_row$p.value
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    p_fdr = stats::p.adjust(p_value, method = "BH"),
    significant_fdr = p_fdr < 0.05
  ) |>
  dplyr::arrange(p_fdr)

readr::write_csv(
  item_results,
  "output/item_outcome_associations_fdr.csv"
)

# Factor-level associations with the same external outcome.
factor_composites <- readr::read_csv(
  "output/factor_composites.csv",
  show_col_types = FALSE
)

factor_dat <- dat |>
  dplyr::select(id, weekly_takeout, age, sex, bmi) |>
  dplyr::left_join(
    factor_composites,
    by = "id"
  )

factor_names <- grep("^Factor\\d+$", names(factor_dat), value = TRUE)

factor_results <- lapply(
  factor_names,
  function(factor_name) {
    model_formula <- stats::as.formula(
      paste0(
        "weekly_takeout ~ scale(",
        factor_name,
        ") + age + sex + bmi"
      )
    )

    fit <- stats::lm(
      model_formula,
      data = factor_dat
    )

    tidy_fit <- broom::tidy(
      fit,
      conf.int = TRUE
    )

    exposure_row <- tidy_fit[
      grepl("^scale\\(", tidy_fit$term),
      ,
      drop = FALSE
    ]

    data.frame(
      factor = factor_name,
      beta = exposure_row$estimate,
      std_error = exposure_row$std.error,
      conf_low = exposure_row$conf.low,
      conf_high = exposure_row$conf.high,
      p_value = exposure_row$p.value
    )
  }
) |>
  dplyr::bind_rows()

readr::write_csv(
  factor_results,
  "output/factor_outcome_associations.csv"
)

message(
  "Item-level and factor-level association analyses complete; ",
  "Benjamini-Hochberg FDR adjustment applied across item tests."
)
