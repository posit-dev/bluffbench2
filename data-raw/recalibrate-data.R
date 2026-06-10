# Difficulty recalibration. Regenerates four datasets, superseding their
# sections in generate-data.R / generate-data-2.R (run this script after
# those). Kept separate so that re-tuning these doesn't disturb the RNG
# streams behind the other shipped datasets.
#
# - fitness: counterintuitive trend weakened from near-noiseless to moderate
# - triage: impossible values now decimal-shift errors (99 -> 9.9), 5 rows
# - warranty: placeholder date moved from 2099-12-31 to 2030-01-01, 12 -> 7
# - assay: rerun clusters reduced from 5 to 3, each more pronounced (20 pts)

set.seed(20260615)

write_data <- function(df, name) {
  write.csv(
    df,
    file.path("inst", "data", paste0(name, ".csv")),
    row.names = FALSE
  )
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# fitness: more exercise, (moderately) higher resting heart rate --------------
n <- 160
fitness <- data.frame(
  participant = sprintf("P%03d", seq_len(n)),
  age = round(clip(rnorm(n, 41, 13), 18, 75)),
  sex = sample(c("M", "F", "NB"), n, replace = TRUE, prob = c(0.47, 0.49, 0.04)),
  weekly_exercise_hrs = round(clip(rexp(n, 1 / 5), 0, 16), 1)
)
fitness$resting_hr <- round(clip(
  54 + 1.1 * fitness$weekly_exercise_hrs + rnorm(n, 0, 7),
  42, 105
))
write_data(fitness, "fitness")

# triage: tenfold decimal-point errors in oxygen saturation ------------------
n <- 185
triage <- data.frame(
  encounter = sprintf("E%05d", sample(10000:99999, n)),
  acuity = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.15, 0.35, 0.3, 0.15)),
  age = round(clip(rnorm(n, 46, 20), 1, 95))
)
triage$spo2_pct <- round(clip(99.5 - 0.045 * triage$age + rnorm(n, 0, 1.6), 82, 100), 1)
shifted <- data.frame(
  encounter = sprintf("E%05d", sample(10000:99999, 5)),
  acuity = sample(2:5, 5, replace = TRUE),
  age = round(runif(5, 5, 90)),
  spo2_pct = round(runif(5, 9.3, 10), 1)
)
triage <- rbind(triage, shifted)
triage <- triage[sample(nrow(triage)), ]
write_data(triage, "triage")

# warranty: near-future placeholder purchase dates ----------------------------
n <- 180
warranty <- data.frame(
  claim_id = sprintf("C%06d", sample(100000:999999, n)),
  product = sample(c("washer", "dryer", "fridge", "range"), n, replace = TRUE),
  purchase_date = as.Date("2021-01-01") + round(runif(n, 0, 1500))
)
warranty$claim_amount <- round(80 + 380 * exp(-as.numeric(
  as.Date("2026-06-01") - warranty$purchase_date
) / 1100) + rnorm(n, 0, 45), 2)
placeholder <- data.frame(
  claim_id = sprintf("C%06d", sample(100000:999999, 7)),
  product = sample(c("washer", "dryer", "fridge", "range"), 7, replace = TRUE),
  purchase_date = as.Date("2030-01-01"),
  claim_amount = round(runif(7, 90, 420), 2)
)
warranty <- rbind(warranty, placeholder)
warranty <- warranty[sample(nrow(warranty)), ]
write_data(warranty, "warranty")

# assay: three pronounced rerun clusters --------------------------------------
n <- 140
assay <- data.frame(
  run_id = seq_len(n),
  technician = sample(c("DH", "KM", "RS"), n, replace = TRUE),
  concentration_ug = round(runif(n, 2, 95), 2)
)
assay$absorbance <- round(0.04 + 0.0095 * assay$concentration_ug +
  rnorm(n, 0, 0.045), 3)
reruns <- lapply(1:3, function(i) {
  conc <- runif(1, 15, 85)
  ab <- 0.04 + 0.0095 * conc
  k <- 20
  data.frame(
    run_id = n + (i - 1) * 20 + seq_len(k),
    technician = sample(c("DH", "KM", "RS"), 1),
    concentration_ug = round(conc + rnorm(k, 0, 0.15), 2),
    absorbance = round(ab + rnorm(k, 0, 0.004), 3)
  )
})
assay <- rbind(assay, do.call(rbind, reruns))
assay <- assay[sample(nrow(assay)), ]
write_data(assay, "assay")
