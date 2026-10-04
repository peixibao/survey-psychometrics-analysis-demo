dat <- readr::read_csv(
  "data/cleaned_questionnaire_data.csv",
  show_col_types = FALSE
)

item_names <- grep("^q\\d{2}$", names(dat), value = TRUE)
items <- as.data.frame(dat[, item_names])

# High endorsement is defined as Likert >= 4.
high_endorsement <- lapply(
  items,
  function(x) {
    ifelse(is.na(x), NA, x >= 4)
  }
) |>
  as.data.frame()

jaccard <- matrix(
  NA_real_,
  nrow = length(item_names),
  ncol = length(item_names),
  dimnames = list(item_names, item_names)
)

for (i in seq_along(item_names)) {
  for (j in seq_along(item_names)) {
    xi <- high_endorsement[[i]]
    xj <- high_endorsement[[j]]

    valid <- !is.na(xi) & !is.na(xj)

    if (sum(valid) == 0) {
      jaccard[i, j] <- NA_real_
    } else {
      intersection <- sum(xi[valid] & xj[valid])
      union <- sum(xi[valid] | xj[valid])

      jaccard[i, j] <- ifelse(
        union == 0,
        0,
        intersection / union
      )
    }
  }
}

diag(jaccard) <- 1
jaccard[is.na(jaccard)] <- 0

readr::write_csv(
  data.frame(item = rownames(jaccard), jaccard, check.names = FALSE),
  "output/item_jaccard_matrix.csv"
)

distance_matrix <- stats::as.dist(1 - jaccard)

hc <- stats::hclust(
  distance_matrix,
  method = "average"
)

factor_composites <- readr::read_csv(
  "output/factor_composites.csv",
  show_col_types = FALSE
)

n_clusters <- max(2, ncol(factor_composites) - 1)

cluster_assignment <- data.frame(
  item = names(stats::cutree(hc, k = n_clusters)),
  cluster = as.integer(stats::cutree(hc, k = n_clusters))
)

readr::write_csv(
  cluster_assignment,
  "output/item_cluster_assignments.csv"
)

saveRDS(
  hc,
  "output/item_hclust.rds"
)

message(
  "Item co-occurrence and hierarchical clustering complete."
)
