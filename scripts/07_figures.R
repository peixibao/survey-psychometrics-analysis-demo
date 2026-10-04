# ---- Factor-loading heatmap ----
loadings <- readr::read_csv(
  "output/efa_loadings.csv",
  show_col_types = FALSE
)

loading_cols <- grep("^Factor\\d+$", names(loadings), value = TRUE)

loading_long <- loadings |>
  dplyr::select(item, dplyr::all_of(loading_cols)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(loading_cols),
    names_to = "factor",
    values_to = "loading"
  )

p_loading <- ggplot2::ggplot(
  loading_long,
  ggplot2::aes(
    x = factor,
    y = factor(item, levels = rev(unique(item))),
    fill = loading
  )
) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.2f", loading)),
    size = 3
  ) +
  ggplot2::scale_fill_gradient2(
    midpoint = 0
  ) +
  ggplot2::labs(
    title = "Exploratory factor loadings",
    x = NULL,
    y = NULL,
    fill = "Loading"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  "output/figure_factor_loadings.png",
  p_loading,
  width = 7,
  height = 8,
  dpi = 180
)

# ---- Jaccard co-occurrence heatmap ----
jaccard_df <- readr::read_csv(
  "output/item_jaccard_matrix.csv",
  show_col_types = FALSE
)

jaccard_long <- jaccard_df |>
  tidyr::pivot_longer(
    cols = -item,
    names_to = "item_2",
    values_to = "jaccard"
  )

p_jaccard <- ggplot2::ggplot(
  jaccard_long,
  ggplot2::aes(
    x = item_2,
    y = factor(item, levels = rev(unique(item))),
    fill = jaccard
  )
) +
  ggplot2::geom_tile() +
  ggplot2::labs(
    title = "Item-level co-occurrence",
    subtitle = "Jaccard similarity for high endorsement (Likert ≥ 4)",
    x = NULL,
    y = NULL,
    fill = "Jaccard"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  "output/figure_item_cooccurrence.png",
  p_jaccard,
  width = 9,
  height = 8,
  dpi = 180
)

# ---- FDR-adjusted association forest plot ----
assoc <- readr::read_csv(
  "output/item_outcome_associations_fdr.csv",
  show_col_types = FALSE
)

assoc$item <- factor(
  assoc$item,
  levels = rev(assoc$item[order(assoc$beta)])
)

p_forest <- ggplot2::ggplot(
  assoc,
  ggplot2::aes(
    x = beta,
    y = item
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = 2
  ) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = conf_low,
      xmax = conf_high
    ),
    height = 0.2
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      shape = significant_fdr
    ),
    size = 2.8
  ) +
  ggplot2::labs(
    title = "Item associations with weekly takeout frequency",
    subtitle = "Adjusted linear models; item scores standardized; FDR across 18 item tests",
    x = "Adjusted beta per 1-SD higher item score",
    y = NULL,
    shape = "FDR < 0.05"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  "output/figure_fdr_associations.png",
  p_forest,
  width = 8,
  height = 7,
  dpi = 180
)

# ---- Hierarchical clustering dendrogram ----
hc <- readRDS("output/item_hclust.rds")

grDevices::png(
  "output/figure_item_dendrogram.png",
  width = 1400,
  height = 900,
  res = 160
)

plot(
  hc,
  main = "Hierarchical clustering of questionnaire items",
  xlab = "",
  sub = "",
  ylab = "Jaccard distance",
  hang = -1
)

grDevices::dev.off()

message("Figures generated.")
