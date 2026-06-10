# Generates the fourth wave of synthetic datasets: survey straight-liners
# (an exact-equality diagonal through a correlated cloud) and a rectangular
# void left by a filtering bug.

set.seed(20260620)

write_data <- function(df, name) {
  write.csv(
    df,
    file.path("inst", "data", paste0(name, ".csv")),
    row.names = FALSE
  )
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# feedback: straight-lined survey responses -----------------------------------
n <- 210
feedback <- data.frame(
  course = sample(
    c("STAT 301", "STAT 412", "DSCI 210", "DSCI 320", "MATH 255"),
    n,
    replace = TRUE
  ),
  modality = sample(c("in-person", "online"), n, replace = TRUE, prob = c(0.65, 0.35)),
  content_rating = round(runif(n, 20, 95), 1)
)
feedback$instructor_rating <- round(clip(
  32 + 0.42 * feedback$content_rating + rnorm(n, 0, 7.5),
  0, 100
), 1)
straight_vals <- round(c(runif(20, 22, 60), runif(14, 60, 95)), 1)
straight <- data.frame(
  course = sample(
    c("STAT 301", "STAT 412", "DSCI 210", "DSCI 320", "MATH 255"),
    34,
    replace = TRUE
  ),
  modality = sample(c("in-person", "online"), 34, replace = TRUE),
  content_rating = straight_vals,
  instructor_rating = straight_vals
)
feedback <- rbind(feedback, straight)
feedback <- feedback[sample(nrow(feedback)), ]
write_data(feedback, "feedback")

# quotes: a filtering bug dropped a rectangular segment -----------------------
n <- 340
quotes <- data.frame(
  region = sample(c("metro", "suburban", "rural"), n, replace = TRUE),
  plan_tier = sample(c("bronze", "silver", "gold"), n, replace = TRUE),
  applicant_age = round(runif(n, 18, 75))
)
quotes$monthly_premium <- round(90 + 2.6 * quotes$applicant_age + rnorm(n, 0, 28), 2)
quotes <- quotes[
  !(quotes$applicant_age > 38 & quotes$applicant_age < 52 &
      quotes$monthly_premium > 185 & quotes$monthly_premium < 265),
]
write_data(quotes, "quotes")
