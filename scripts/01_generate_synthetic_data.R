set.seed(2026)

n <- 900
item_names <- sprintf("q%02d", 1:18)

# Three correlated latent questionnaire dimensions.
latent_cor <- matrix(
  c(
    1.00,  0.30, -0.25,
    0.30,  1.00, -0.20,
   -0.25, -0.20,  1.00
  ),
  nrow = 3,
  byrow = TRUE
)

latent <- MASS::mvrnorm(
  n = n,
  mu = c(0, 0, 0),
  Sigma = latent_cor
)

colnames(latent) <- c(
  "reward_sensitivity",
  "emotional_eating",
  "self_regulation"
)

# Six items per dimension, with a small amount of realistic cross-loading.
lambda <- matrix(0, nrow = 18, ncol = 3)
lambda[1:6, 1] <- c(0.82, 0.76, 0.71, 0.68, 0.74, 0.64)
lambda[7:12, 2] <- c(0.79, 0.73, 0.70, 0.75, 0.66, 0.63)
lambda[13:18, 3] <- c(0.81, 0.77, 0.69, 0.72, 0.67, 0.62)

lambda[6, 2] <- 0.22
lambda[12, 1] <- 0.20
lambda[18, 2] <- -0.18

resid_sd <- sqrt(pmax(0.20, 1 - rowSums(lambda^2)))

continuous_items <- latent %*% t(lambda) +
  matrix(rnorm(n * 18), nrow = n, ncol = 18) *
  matrix(rep(resid_sd, each = n), nrow = n, ncol = 18)

# Convert continuous responses to 1-5 Likert responses.
cut_points <- c(-Inf, -0.85, -0.20, 0.35, 0.95, Inf)

likert <- apply(
  continuous_items,
  2,
  function(x) {
    as.integer(
      cut(
        x,
        breaks = cut_points,
        labels = 1:5,
        include.lowest = TRUE
      )
    )
  }
)

colnames(likert) <- item_names

# Participant characteristics and external variables.
age <- pmin(pmax(round(rnorm(n, mean = 21.2, sd = 1.8)), 18), 28)
sex <- sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.58, 0.42))
bmi <- round(pmin(pmax(rnorm(n, mean = 22.6, sd = 3.2), 16.0), 36.0), 1)

weekly_takeout <- round(
  3.2 +
    1.10 * latent[, "reward_sensitivity"] +
    0.65 * latent[, "emotional_eating"] -
    0.45 * latent[, "self_regulation"] +
    0.03 * (bmi - 22.5) +
    rnorm(n, 0, 1.4),
  1
)
weekly_takeout <- pmin(pmax(weekly_takeout, 0), 14)

stress_score <- round(
  18 +
    3.4 * latent[, "emotional_eating"] -
    1.2 * latent[, "self_regulation"] +
    rnorm(n, 0, 4.0),
  1
)
stress_score <- pmin(pmax(stress_score, 0), 40)

completion_time_min <- round(rlnorm(n, log(8.5), 0.32), 1)
attention_check <- rep(1L, n)

# Add a small number of intentionally low-quality responses.
inattentive_ids <- sample(seq_len(n), size = round(0.05 * n))
straightline_ids <- inattentive_ids[seq_len(floor(length(inattentive_ids) / 2))]
random_ids <- setdiff(inattentive_ids, straightline_ids)

for (i in straightline_ids) {
  likert[i, ] <- sample(2:4, 1)
}

for (i in random_ids) {
  likert[i, ] <- sample(1:5, 18, replace = TRUE)
}

attention_check[inattentive_ids] <- rbinom(length(inattentive_ids), 1, 0.25)
completion_time_min[inattentive_ids] <- round(runif(length(inattentive_ids), 1.0, 3.0), 1)

# Add modest item-level missingness among otherwise attentive respondents.
attentive_ids <- setdiff(seq_len(n), inattentive_ids)

for (j in seq_len(ncol(likert))) {
  miss_j <- attentive_ids[runif(length(attentive_ids)) < 0.025]
  likert[miss_j, j] <- NA_integer_
}

synthetic_data <- data.frame(
  id = sprintf("P%04d", seq_len(n)),
  age = age,
  sex = sex,
  bmi = bmi,
  weekly_takeout = weekly_takeout,
  stress_score = stress_score,
  completion_time_min = completion_time_min,
  attention_check = attention_check,
  likert,
  check.names = FALSE
)

readr::write_csv(
  synthetic_data,
  "data/synthetic_questionnaire_data.csv",
  na = ""
)

message(
  "Synthetic questionnaire dataset created: ",
  nrow(synthetic_data),
  " participants, ",
  length(item_names),
  " Likert items."
)
