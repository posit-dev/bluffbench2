# Generates the second wave of synthetic datasets: oddities chosen because
# they consistently slip past models in screening (heaped values, imputed
# points, pseudoreplication, stuck sensors, swapped columns, impossible
# zeros, crossing subgroups, placeholder dates).

set.seed(20260611)

write_data <- function(df, name) {
  write.csv(
    df,
    file.path("inst", "data", paste0(name, ".csv")),
    row.names = FALSE
  )
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# clinic: terminal digit preference in systolic blood pressure ----------------
n <- 210
clinic <- data.frame(
  patient_id = sprintf("MRN%05d", sample(10000:99999, n)),
  clinic_site = sample(c("Downtown", "Westside", "Northgate"), n, replace = TRUE),
  sex = sample(c("M", "F"), n, replace = TRUE),
  age = round(clip(rnorm(n, 56, 15), 20, 90))
)
clinic$systolic <- 95 + 0.55 * clinic$age + rnorm(n, 0, 9)
heaped <- sample(n, round(n * 0.55))
clinic$systolic[heaped] <- round(clinic$systolic[heaped] / 10) * 10
clinic$systolic <- round(clinic$systolic)
write_data(clinic, "clinic")

# screentime: self-reported minutes heaped at half-hours ----------------------
n <- 200
screentime <- data.frame(
  respondent = seq_len(n),
  region = sample(c("urban", "suburban", "rural"), n, replace = TRUE),
  age = round(clip(rnorm(n, 34, 12), 13, 70))
)
screentime$daily_minutes <- clip(290 - 2.4 * screentime$age + rnorm(n, 0, 35), 10, 420)
heaped <- sample(n, round(n * 0.6))
screentime$daily_minutes[heaped] <-
  round(screentime$daily_minutes[heaped] / 30) * 30
screentime$daily_minutes <- round(screentime$daily_minutes)
write_data(screentime, "screentime")

# labs: regression-imputed cholesterol values ---------------------------------
n <- 150
labs <- data.frame(
  specimen = sprintf("L-%04d", seq_len(n)),
  fasting = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.8, 0.2)),
  bmi = round(runif(n, 18, 42), 1)
)
labs$cholesterol <- round(120 + 2.6 * labs$bmi + rnorm(n, 0, 18), 1)
imp_bmi <- round(runif(55, 20, 40), 1)
labs_imp <- data.frame(
  specimen = sprintf("L-%04d", n + seq_along(imp_bmi)),
  fasting = sample(c("yes", "no"), length(imp_bmi), replace = TRUE),
  bmi = imp_bmi,
  cholesterol = round(120 + 2.6 * imp_bmi, 1)
)
labs <- rbind(labs, labs_imp)
labs <- labs[sample(nrow(labs)), ]
write_data(labs, "labs")

# wellbeing: imputed stress scores ---------------------------------------------
n <- 160
wellbeing <- data.frame(
  participant = sprintf("W%03d", seq_len(n)),
  employment = sample(
    c("full-time", "part-time", "student", "retired"),
    n,
    replace = TRUE
  ),
  sleep_hours = round(runif(n, 4, 10), 1)
)
wellbeing$stress_score <- round(clip(95 - 7.5 * wellbeing$sleep_hours + rnorm(n, 0, 8), 5, 95), 1)
imp_sleep <- round(runif(50, 4.5, 9.5), 1)
wb_imp <- data.frame(
  participant = sprintf("W%03d", n + seq_along(imp_sleep)),
  employment = sample(
    c("full-time", "part-time", "student", "retired"),
    length(imp_sleep),
    replace = TRUE
  ),
  sleep_hours = imp_sleep,
  stress_score = round(95 - 7.5 * imp_sleep, 1)
)
wellbeing <- rbind(wellbeing, wb_imp)
wellbeing <- wellbeing[sample(nrow(wellbeing)), ]
write_data(wellbeing, "wellbeing")

# tags: a few tagged fish weighed many times ----------------------------------
n <- 130
tags <- data.frame(
  tag_id = sprintf("T%04d", sample(1000:9999, n)),
  reach = sample(c("upper", "middle", "lower"), n, replace = TRUE),
  length_cm = round(runif(n, 12, 60), 1)
)
tags$weight_g <- round(10.5 * tags$length_cm^1.6 * exp(rnorm(n, 0, 0.18)) / 10)
repeats <- lapply(1:6, function(i) {
  len <- runif(1, 20, 52)
  wt <- 10.5 * len^1.6 / 10
  k <- 11
  data.frame(
    tag_id = sprintf("T%04d", sample(1000:9999, 1)),
    reach = sample(c("upper", "middle", "lower"), 1),
    length_cm = round(len + rnorm(k, 0, 0.25), 1),
    weight_g = round(wt * exp(rnorm(k, 0, 0.012)))
  )
})
tags <- rbind(tags, do.call(rbind, repeats))
tags <- tags[sample(nrow(tags)), ]
write_data(tags, "tags")

# assay: technician re-ran the same specimens ---------------------------------
n <- 140
assay <- data.frame(
  run_id = seq_len(n),
  technician = sample(c("DH", "KM", "RS"), n, replace = TRUE),
  concentration_ug = round(runif(n, 2, 95), 2)
)
assay$absorbance <- round(0.04 + 0.0095 * assay$concentration_ug +
  rnorm(n, 0, 0.045), 3)
reruns <- lapply(1:5, function(i) {
  conc <- runif(1, 15, 85)
  ab <- 0.04 + 0.0095 * conc
  k <- 12
  data.frame(
    run_id = n + (i - 1) * 12 + seq_len(k),
    technician = sample(c("DH", "KM", "RS"), 1),
    concentration_ug = round(conc + rnorm(k, 0, 0.15), 2),
    absorbance = round(ab + rnorm(k, 0, 0.004), 3)
  )
})
assay <- rbind(assay, do.call(rbind, reruns))
assay <- assay[sample(nrow(assay)), ]
write_data(assay, "assay")

# greenhouse: stuck humidity sensor -------------------------------------------
n <- 190
greenhouse <- data.frame(
  bay = sample(paste0("Bay ", 1:6), n, replace = TRUE),
  fan_on = sample(c("yes", "no"), n, replace = TRUE),
  temperature_c = round(runif(n, 16, 34), 1)
)
greenhouse$humidity_pct <- round(clip(96 - 1.7 * greenhouse$temperature_c +
  rnorm(n, 0, 6), 20, 100), 1)
stuck <- data.frame(
  bay = sample(paste0("Bay ", 1:6), 32, replace = TRUE),
  fan_on = sample(c("yes", "no"), 32, replace = TRUE),
  temperature_c = round(runif(32, 17, 33), 1),
  humidity_pct = 54.2
)
greenhouse <- rbind(greenhouse, stuck)
greenhouse <- greenhouse[sample(nrow(greenhouse)), ]
write_data(greenhouse, "greenhouse")

# meter: flow meter frozen at one reading -------------------------------------
n <- 200
meter <- data.frame(
  station = sample(c("inlet", "outlet"), n, replace = TRUE),
  day = round(runif(n, 1, 90), 1)
)
meter$flow_m3h <- round(42 + 14 * sin(meter$day / 9) + 0.1 * meter$day +
  rnorm(n, 0, 3.2), 2)
frozen <- data.frame(
  station = sample(c("inlet", "outlet"), 30, replace = TRUE),
  day = round(runif(30, 25, 80), 1),
  flow_m3h = 51.37
)
meter <- rbind(meter, frozen)
meter <- meter[sample(nrow(meter)), ]
write_data(meter, "meter")

# shipping: weight and cost columns swapped for some rows ---------------------
n <- 180
shipping <- data.frame(
  order_id = sample(100000:999999, n),
  carrier = sample(c("UPX", "FedDel", "Postal"), n, replace = TRUE),
  weight_kg = round(runif(n, 0.4, 30), 2)
)
shipping$cost_usd <- round(7 + 0.42 * shipping$weight_kg + rnorm(n, 0, 1.1), 2)
swap <- sample(n, 26)
tmp <- shipping$weight_kg[swap]
shipping$weight_kg[swap] <- shipping$cost_usd[swap]
shipping$cost_usd[swap] <- tmp
write_data(shipping, "shipping")

# weather: morning and afternoon temps swapped for some rows ------------------
n <- 170
weather <- data.frame(
  station_id = sprintf("WX%02d", sample(1:25, n, replace = TRUE)),
  month = sample(month.abb[4:9], n, replace = TRUE),
  morning_c = round(runif(n, 6, 22), 1)
)
weather$afternoon_c <- round(weather$morning_c + 7 + rnorm(n, 0, 1.4), 1)
swap <- sample(n, 24)
tmp <- weather$morning_c[swap]
weather$morning_c[swap] <- weather$afternoon_c[swap]
weather$afternoon_c[swap] <- tmp
write_data(weather, "weather")

# tracker: resting heart rate of zero from sensor dropouts --------------------
n <- 190
tracker <- data.frame(
  user = sprintf("U%04d", sample(1000:9999, n)),
  device = sample(c("band2", "band3", "watchX"), n, replace = TRUE),
  daily_steps = round(clip(rnorm(n, 8200, 3300), 500, 22000))
)
tracker$resting_hr <- round(clip(76 - 0.0011 * tracker$daily_steps +
  rnorm(n, 0, 5), 44, 100))
dropouts <- data.frame(
  user = sprintf("U%04d", sample(1000:9999, 14)),
  device = sample(c("band2", "band3", "watchX"), 14, replace = TRUE),
  daily_steps = round(runif(14, 1500, 20000)),
  resting_hr = 0
)
tracker <- rbind(tracker, dropouts)
tracker <- tracker[sample(nrow(tracker)), ]
write_data(tracker, "tracker")

# triage: oxygen saturation of zero in walk-in patients -----------------------
n <- 185
triage <- data.frame(
  encounter = sprintf("E%05d", sample(10000:99999, n)),
  acuity = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.15, 0.35, 0.3, 0.15)),
  age = round(clip(rnorm(n, 46, 20), 1, 95))
)
triage$spo2_pct <- round(clip(99.5 - 0.045 * triage$age + rnorm(n, 0, 1.6), 82, 100), 1)
missing_coded <- data.frame(
  encounter = sprintf("E%05d", sample(10000:99999, 12)),
  acuity = sample(2:5, 12, replace = TRUE),
  age = round(runif(12, 5, 90)),
  spo2_pct = 0
)
triage <- rbind(triage, missing_coded)
triage <- triage[sample(nrow(triage)), ]
write_data(triage, "triage")

# rentals: two crossing bands in distance vs price ----------------------------
n <- 190
luxury <- runif(n) < 0.28
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

# growth: two species with opposite light responses mixed together ------------
n <- 180
shade <- runif(n) < 0.3
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

# memberships: epoch-default join dates ---------------------------------------
n <- 185
memberships <- data.frame(
  member_id = sprintf("M%05d", sample(10000:99999, n)),
  tier = sample(c("basic", "plus", "family"), n, replace = TRUE),
  join_date = as.Date("2022-06-01") + round(runif(n, 0, 1200))
)
memberships$visits_per_month <- round(clip(
  2 + 8 * exp(-as.numeric(as.Date("2026-06-01") - memberships$join_date) / 900) +
    rnorm(n, 0, 1.6),
  0, 16
), 1)
epoch_rows <- data.frame(
  member_id = sprintf("M%05d", sample(10000:99999, 13)),
  tier = sample(c("basic", "plus", "family"), 13, replace = TRUE),
  join_date = as.Date("1970-01-01"),
  visits_per_month = round(runif(13, 1, 10), 1)
)
memberships <- rbind(memberships, epoch_rows)
memberships <- memberships[sample(nrow(memberships)), ]
write_data(memberships, "memberships")

# warranty: placeholder far-future purchase dates -----------------------------
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
  claim_id = sprintf("C%06d", sample(100000:999999, 12)),
  product = sample(c("washer", "dryer", "fridge", "range"), 12, replace = TRUE),
  purchase_date = as.Date("2099-12-31"),
  claim_amount = round(runif(12, 90, 420), 2)
)
warranty <- rbind(warranty, placeholder)
warranty <- warranty[sample(nrow(warranty)), ]
write_data(warranty, "warranty")
