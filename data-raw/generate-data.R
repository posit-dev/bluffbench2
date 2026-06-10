# Generates the synthetic datasets shipped in inst/data/, one section per
# dataset. Each dataset contains a couple of unremarkable columns (for the
# lull plots) and a pair of columns whose joint distribution contains the
# artifact. Each section sets its own seed so datasets can be re-tuned
# independently.

write_data <- function(df, name) {
  write.csv(
    df,
    file.path("inst", "data", paste0(name, ".csv")),
    row.names = FALSE
  )
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# assay: a few specimens re-run many times (pseudoreplication) ----------------
set.seed(101)
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

# bridges: the same few bridges inspected repeatedly --------------------------
set.seed(102)
n <- 135
bridges <- data.frame(
  inspection_id = sprintf("INS%05d", sample(10000:99999, n)),
  district = sample(c("north", "south", "east", "west"), n, replace = TRUE),
  span_m = round(runif(n, 18, 220), 1)
)
bridges$deflection_mm <- round(2 + 0.085 * bridges$span_m *
  exp(rnorm(n, 0, 0.18)), 2)
repeats <- lapply(1:5, function(i) {
  span <- runif(1, 40, 190)
  defl <- 2 + 0.085 * span
  k <- 12
  data.frame(
    inspection_id = sprintf("INS%05d", sample(10000:99999, k)),
    district = sample(c("north", "south", "east", "west"), 1),
    span_m = round(span + rnorm(k, 0, 0.3), 1),
    deflection_mm = round(defl * exp(rnorm(k, 0, 0.015)), 2)
  )
})
bridges <- rbind(bridges, do.call(rbind, repeats))
bridges <- bridges[sample(nrow(bridges)), ]
write_data(bridges, "bridges")

# claims: a bad join repeats a few exact incomes (vertical strings) -----------
set.seed(103)
n <- 180
claims <- data.frame(
  claim_type = sample(c("auto", "home", "umbrella"), n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
  region = sample(c("north", "south", "coastal"), n, replace = TRUE),
  claimant_income_k = round(runif(n, 28, 140), 2)
)
claims$claim_amount_k <- round(clip(0.9 + 0.012 * claims$claimant_income_k +
  rnorm(n, 0, 0.55), 0.2, 4), 2)
strings <- do.call(rbind, lapply(1:6, function(i) {
  inc <- round(runif(1, 35, 130), 2)
  data.frame(
    claim_type = sample(c("auto", "home", "umbrella"), 11, replace = TRUE),
    region = sample(c("north", "south", "coastal"), 1),
    claimant_income_k = rep(inc, 11),
    claim_amount_k = round(clip(0.9 + 0.012 * inc + rnorm(11, 0, 0.55), 0.2, 4), 2)
  )
}))
claims <- rbind(claims, strings)
claims <- claims[sample(nrow(claims)), ]
write_data(claims, "claims")

# clinic: terminal digit preference in systolic blood pressure ----------------
set.seed(104)
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

# energy: regression-imputed heating usage ------------------------------------
set.seed(105)
n <- 150
energy <- data.frame(
  meter_id = sprintf("MTR%04d", sample(1000:9999, n)),
  dwelling = sample(c("apartment", "detached", "townhouse"), n, replace = TRUE),
  outdoor_temp_c = round(runif(n, -8, 18), 1)
)
energy$heating_kwh <- round(38 - 1.45 * energy$outdoor_temp_c + rnorm(n, 0, 4.2), 2)
imp_t <- round(sort(runif(55, -7, 17)), 1)
energy_imp <- data.frame(
  meter_id = sprintf("MTR%04d", sample(1000:9999, length(imp_t))),
  dwelling = sample(c("apartment", "detached", "townhouse"), length(imp_t), replace = TRUE),
  outdoor_temp_c = imp_t,
  heating_kwh = round(38 - 1.45 * imp_t, 2)
)
energy <- rbind(energy, energy_imp)
energy <- energy[sample(nrow(energy)), ]
write_data(energy, "energy")

# feedback: straight-lined survey responses (exact y = x diagonal) ------------
set.seed(106)
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

# greenhouse: stuck humidity sensor hidden inside the band --------------------
set.seed(107)
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

# growth: two species with opposite light responses mixed together ------------
set.seed(108)
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

# labs: regression-imputed cholesterol values ---------------------------------
set.seed(109)
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

# ponds: stuck oxygen probe hidden inside the band ----------------------------
set.seed(110)
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

# quotes: a filtering bug dropped a rectangular segment -----------------------
set.seed(111)
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

# rentals: two crossing bands in distance vs price ----------------------------
set.seed(112)
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

# river: drifting turbidity sensor (smooth in-band chain) ---------------------
set.seed(113)
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

# screentime: self-reported minutes heaped at half-hours ----------------------
set.seed(114)
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

# seals: one batch's measurements are suspiciously precise --------------------
set.seed(115)
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

# shipping: weight and cost columns swapped for some rows ---------------------
set.seed(116)
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

# stores: negative margins folded positive ------------------------------------
set.seed(117)
n <- 230
stores <- data.frame(
  store_id = sprintf("S%04d", sample(1000:9999, n)),
  region = sample(c("northeast", "southeast", "central", "west"), n, replace = TRUE),
  floor_area_sqm = round(runif(n, 40, 320), 1)
)
margin <- -8 + 0.075 * stores$floor_area_sqm + rnorm(n, 0, 9)
stores$weekly_margin_k <- round(abs(margin), 2)
write_data(stores, "stores")

# tags: a few tagged fish weighed many times ----------------------------------
set.seed(118)
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

# weather: balanced mirrored bands from swapped columns -----------------------
set.seed(119)
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

# wellbeing: imputed stress scores ---------------------------------------------
set.seed(120)
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
