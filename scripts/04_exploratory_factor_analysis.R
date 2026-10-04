dat <- readr::read_csv(
  "data/cleaned_questionnaire_data.csv",
  show_col_types = FALSE
)

item_names <- grep("^q\\d{2}$", names(dat), value = TRUE)
items <- as.data.frame(dat[, item_names])

# Median imputation is used only to obtain a stable polychoric correlation
# matrix for this synthetic demonstration.
items_for_cor <- items

for (item in item_names) {
  med <- stats::median(items_for_cor[[item]], na.rm = TRUE)
  items_for_cor[[item]][is.na(items_for_cor[[item]])] <- med
}

poly_obj <- suppressWarnings(
  psych::polychoric(items_for_cor)
)

rho <- poly_obj$rho

kmo_result <- psych::KMO(rho)
bartlett_result <- psych::cortest.bartlett(
  rho,
  n = nrow(items_for_cor)
)

parallel_result <- suppressMessages(
  psych::fa.parallel(
    rho,
    n.obs = nrow(items_for_cor),
    fa = "fa",
    fm = "minres",
    plot = FALSE
  )
)

nfactors <- parallel_result$nfact

if (is.null(nfactors) || length(nfactors) == 0 || is.na(nfactors)) {
  nfactors <- 3
}

nfactors <- max(1, min(6, as.integer(nfactors)))

efa_fit <- psych::fa(
  rho,
  nfactors = nfactors,
  n.obs = nrow(items_for_cor),
  rotate = "oblimin",
  fm = "minres"
)

loading_matrix <- unclass(efa_fit$loadings)
colnames(loading_matrix) <- paste0("Factor", seq_len(ncol(loading_matrix)))

loadings_df <- data.frame(
  item = rownames(loading_matrix),
  loading_matrix,
  communality = as.numeric(efa_fit$communality),
  uniqueness = as.numeric(efa_fit$uniquenesses),
  check.names = FALSE
)

primary_factor <- apply(
  abs(loading_matrix),
  1,
  which.max
)

loadings_df$primary_factor <- paste0("Factor", primary_factor)

readr::write_csv(
  loadings_df,
  "output/efa_loadings.csv"
)

# Reliability within item sets assigned to their strongest factor.
reliability <- lapply(
  seq_len(nfactors),
  function(k) {
    factor_name <- paste0("Factor", k)
    factor_items <- loadings_df$item[
      loadings_df$primary_factor == factor_name
    ]

    alpha_value <- NA_real_

    if (length(factor_items) >= 2) {
      alpha_value <- suppressWarnings(
        psych::alpha(
          items[, factor_items, drop = FALSE],
          warnings = FALSE,
          check.keys = FALSE
        )$total$raw_alpha
      )
    }

    data.frame(
      factor = factor_name,
      n_items = length(factor_items),
      items = paste(factor_items, collapse = ", "),
      cronbach_alpha = alpha_value
    )
  }
) |>
  dplyr::bind_rows()

readr::write_csv(
  reliability,
  "output/reliability_by_factor.csv"
)

# Transparent factor composites based on primary-loading item groups.
factor_composites <- data.frame(id = dat$id)

for (k in seq_len(nfactors)) {
  factor_name <- paste0("Factor", k)
  factor_items <- loadings_df$item[
    loadings_df$primary_factor == factor_name
  ]

  if (length(factor_items) > 0) {
    score <- rowMeans(
      dat[, factor_items, drop = FALSE],
      na.rm = TRUE
    )
    score[is.nan(score)] <- NA_real_
    factor_composites[[factor_name]] <- score
  }
}

readr::write_csv(
  factor_composites,
  "output/factor_composites.csv"
)

variance_accounted <- efa_fit$Vaccounted

summary_lines <- c(
  paste0("Participants included: ", nrow(items_for_cor)),
  paste0("Items included: ", length(item_names)),
  paste0("KMO overall MSA: ", round(kmo_result$MSA, 3)),
  paste0(
    "Bartlett test: chi-square = ",
    round(bartlett_result$chisq, 2),
    ", df = ",
    bartlett_result$df,
    ", p = ",
    format.pval(bartlett_result$p.value, digits = 3)
  ),
  paste0("Parallel analysis suggested factors: ", nfactors),
  "",
  "Variance accounted:",
  capture.output(print(round(variance_accounted, 3)))
)

writeLines(
  summary_lines,
  "output/efa_summary.txt"
)

message(
  "Exploratory factor analysis complete. Parallel analysis suggested ",
  nfactors,
  " factors."
)
