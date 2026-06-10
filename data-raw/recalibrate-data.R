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

# round 2 ----------------------------------------------------------------------
# - weather: swapped rows rebalanced to ~45% and bands moved closer (+5)
# - rentals/growth: minority crossing band halved
# - seals: too-clean batch made even tighter (sd 0.25 -> 0.08)

# weather: two balanced mirrored bands from swapped columns -------------------
n <- 170
weather <- data.frame(
  station_id = sprintf("WX%02d", sample(1:25, n, replace = TRUE)),
  month = sample(month.abb[4:9], n, replace = TRUE),
  morning_c = round(runif(n, 6, 22), 1)
)
weather$afternoon_c <- round(weather$morning_c + 5 + rnorm(n, 0, 1.2), 1)
swap <- sample(n, round(0.45 * n))
tmp <- weather$morning_c[swap]
weather$morning_c[swap] <- weather$afternoon_c[swap]
weather$afternoon_c[swap] <- tmp
write_data(weather, "weather")

# rentals: smaller crossing band -----------------------------------------------
n <- 190
luxury <- runif(n) < 0.14
rentals <- data.frame(
  listing = sample(10000:99999, n),
  bedrooms = sample(1:4, n, replace = TRUE),
  distance_km = round(runif(n, 0.3, 18), 2)
)
rentals$rent_usd <- round(ifelse(
  luxury,
  1300 + 95 * rentals$distance_km,
  2900 - 80 * rentals$distance_km
) + rnorm(n, 0, 130))
write_data(rentals, "rentals")

# growth: smaller crossing band ------------------------------------------------
n <- 180
shade <- runif(n) < 0.15
growth <- data.frame(
  tray = sample(paste0("T", 1:10), n, replace = TRUE),
  watering = sample(c("daily", "alternate"), n, replace = TRUE),
  light_hours = round(runif(n, 4, 16), 1)
)
growth$height_cm <- round(ifelse(
  shade,
  30 - 1.3 * growth$light_hours,
  2 + 1.7 * growth$light_hours
) + rnorm(n, 0, 2.2), 1)
write_data(growth, "growth")

# seals: too-clean batch tightened ---------------------------------------------
n <- 165
seals <- data.frame(
  batch = sample(paste0("B", 1:8), n, replace = TRUE),
  inspector = sample(c("AL", "JM", "TS"), n, replace = TRUE),
  pressure_psi = round(runif(n, 10, 90), 1)
)
seals$leak_rate_mlh <- round(2 + 0.31 * seals$pressure_psi + rnorm(n, 0, 3.4), 2)
clean <- data.frame(
  batch = "B9",
  inspector = sample(c("AL", "JM", "TS"), 45, replace = TRUE),
  pressure_psi = round(runif(45, 15, 85), 1)
)
clean$leak_rate_mlh <- round(2 + 0.31 * clean$pressure_psi + rnorm(45, 0, 0.08), 2)
seals <- rbind(seals, clean)
seals <- seals[sample(nrow(seals)), ]
write_data(seals, "seals")

# round 3 ----------------------------------------------------------------------
# Hardening the samples Opus 4.8 medium got correct in both epochs:
# - greenhouse/ponds: stuck values now sit where the trend passes through
#   them, over a limited x range, with fewer points (the flat run hides
#   inside the band instead of cutting across it)
# - river: drift arc lowered into the cloud's center, shortened
# - weather: mirrored bands moved closer together (+/-3, tighter noise)

# greenhouse: stuck humidity hidden inside the band ---------------------------
n <- 190
greenhouse <- data.frame(
  bay = sample(paste0("Bay ", 1:6), n, replace = TRUE),
  fan_on = sample(c("yes", "no"), n, replace = TRUE),
  temperature_c = round(runif(n, 16, 34), 1)
)
greenhouse$humidity_pct <- round(clip(96 - 1.7 * greenhouse$temperature_c +
  rnorm(n, 0, 6), 20, 100), 1)
stuck <- data.frame(
  bay = sample(paste0("Bay ", 1:6), 16, replace = TRUE),
  fan_on = sample(c("yes", "no"), 16, replace = TRUE),
  temperature_c = round(runif(16, 21.5, 28), 1),
  humidity_pct = 54.2
)
greenhouse <- rbind(greenhouse, stuck)
greenhouse <- greenhouse[sample(nrow(greenhouse)), ]
write_data(greenhouse, "greenhouse")

# ponds: stuck oxygen hidden inside the band ----------------------------------
n <- 185
ponds <- data.frame(
  pond = sample(paste0("P", 1:8), n, replace = TRUE),
  fed_today = sample(c("yes", "no"), n, replace = TRUE),
  water_temp_c = round(runif(n, 12, 30), 1)
)
ponds$dissolved_o2_mgl <- round(clip(13.5 - 0.22 * ponds$water_temp_c +
  rnorm(n, 0, 0.7), 4, 14), 2)
stuck <- data.frame(
  pond = sample(paste0("P", 1:8), 15, replace = TRUE),
  fed_today = sample(c("yes", "no"), 15, replace = TRUE),
  water_temp_c = round(runif(15, 19.5, 26.5), 1),
  dissolved_o2_mgl = 8.45
)
ponds <- rbind(ponds, stuck)
ponds <- ponds[sample(nrow(ponds)), ]
write_data(ponds, "ponds")

# river: shorter, in-band drift filament --------------------------------------
n <- 200
river <- data.frame(
  hour = round(sort(runif(n, 0, 240)), 2),
  weekday = NA_character_,
  battery_v = NA_real_
)
river$weekday <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[
  (floor(river$hour / 24) %% 7) + 1
]
river$battery_v <- round(12.6 - 0.004 * river$hour + rnorm(n, 0, 0.15), 2)
river$turbidity_ntu <- round(18 + 4 * sin(river$hour / 38) + rnorm(n, 0, 3.2), 2)

drift_hours <- seq(105, 140, length.out = 18)
drift_vals <- 19 - 0.07 * (drift_hours - 105) +
  cumsum(rnorm(18, 0, 0.28))
drift <- data.frame(
  hour = round(drift_hours, 2),
  weekday = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[
    (floor(drift_hours / 24) %% 7) + 1
  ],
  battery_v = round(12.6 - 0.004 * drift_hours + rnorm(18, 0, 0.15), 2),
  turbidity_ntu = round(drift_vals, 2)
)
river <- rbind(river, drift)
river <- river[order(river$hour), ]
write_data(river, "river")

# weather: mirrored bands closer together -------------------------------------
n <- 170
weather <- data.frame(
  station_id = sprintf("WX%02d", sample(1:25, n, replace = TRUE)),
  month = sample(month.abb[4:9], n, replace = TRUE),
  morning_c = round(runif(n, 6, 22), 1)
)
weather$afternoon_c <- round(weather$morning_c + 3 + rnorm(n, 0, 1.0), 1)
swap <- sample(n, round(0.45 * n))
tmp <- weather$morning_c[swap]
weather$morning_c[swap] <- weather$afternoon_c[swap]
weather$afternoon_c[swap] <- tmp
write_data(weather, "weather")
