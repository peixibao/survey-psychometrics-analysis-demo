dat <- readr::read_csv(
  "data/synthetic_questionnaire_data.csv",
  show_col_types = FALSE
)

item_names <- grep("^q\\d{2}$", names(dat), value = TRUE)
item_matrix <- as.data.frame(dat[, item_names])

longest_run <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  max(rle(x)$lengths)
}

missing_prop <- apply(item_matrix, 1, function(x) mean(is.na(x)))

response_sd <- apply(
  item_matrix,
  1,
  function(x) {
    if (sum(!is.na(x)) < 2) return(NA_real_)
    stats::sd(as.numeric(x), na.rm = TRUE)
  }
)

max_run <- apply(item_matrix, 1, longest_run)

quality <- data.frame(
  id = dat$id,
  missing_prop = missing_prop,
  response_sd = response_sd,
  max_identical_run = max_run,
  completion_time_min = dat$completion_time_min,
  attention_check = dat$attention_check
)

quality <- quality |>
  dplyr::mutate(
    flag_missing = missing_prop > 0.20,
    flag_straightline = (!is.na(response_sd) & response_sd < 0.15) |
      (!is.na(max_identical_run) & max_identical_run >= 15),
    flag_speed = completion_time_min < 2.5,
    flag_attention = attention_check != 1,
    quality_flag = flag_missing | flag_straightline | flag_speed | flag_attention
  )

clean_dat <- dat |>
  dplyr::left_join(
    quality |>
      dplyr::select(id, quality_flag),
    by = "id"
  ) |>
  dplyr::filter(!quality_flag) |>
  dplyr::select(-quality_flag)

quality_summary <- data.frame(
  metric = c(
    "Total participants",
    "Flagged: excessive missingness",
    "Flagged: straightlining",
    "Flagged: unusually fast completion",
    "Flagged: failed attention check",
    "Flagged: any quality issue",
    "Retained for analysis"
  ),
  n = c(
    nrow(dat),
    sum(quality$flag_missing),
    sum(quality$flag_straightline),
    sum(quality$flag_speed),
    sum(quality$flag_attention),
    sum(quality$quality_flag),
    nrow(clean_dat)
  )
)

quality_summary$percent <- round(
  100 * quality_summary$n / nrow(dat),
  1
)

readr::write_csv(
  quality,
  "output/participant_quality_flags.csv"
)

readr::write_csv(
  quality_summary,
  "output/quality_check_summary.csv"
)

readr::write_csv(
  clean_dat,
  "data/cleaned_questionnaire_data.csv",
  na = ""
)

message(
  "Response-quality checks complete. Retained ",
  nrow(clean_dat),
  " of ",
  nrow(dat),
  " participants."
)
