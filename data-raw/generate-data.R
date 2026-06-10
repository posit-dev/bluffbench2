# Generates the synthetic datasets shipped in inst/data/. Each dataset
# contains a couple of unremarkable columns (for the lull plots) and a pair
# of columns whose joint distribution contains the artifact.

set.seed(20260610)

write_data <- function(df, name) {
  write.csv(
    df,
    file.path("inst", "data", paste0(name, ".csv")),
    row.names = FALSE
  )
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# thymoma: larger tumors, longer survival ------------------------------------
n <- 150
thymoma <- data.frame(
  patient_id = sprintf("PT-%04d", sample(1000:9999, n)),
  age = round(clip(rnorm(n, 62, 12), 28, 88)),
  sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.55, 0.45)),
  chemo = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.4, 0.6)),
  tumor_size_cm = round(runif(n, 0.8, 9.2), 1)
)
thymoma$survival_months <- round(clip(
  6 + 9.5 * thymoma$tumor_size_cm + rnorm(n, 0, 9),
  1, 120
))
write_data(thymoma, "thymoma")

# fitness: more exercise, higher resting heart rate ---------------------------
n <- 160
fitness <- data.frame(
  participant = sprintf("P%03d", seq_len(n)),
  age = round(clip(rnorm(n, 41, 13), 18, 75)),
  sex = sample(c("M", "F", "NB"), n, replace = TRUE, prob = c(0.47, 0.49, 0.04)),
  weekly_exercise_hrs = round(clip(rexp(n, 1 / 5), 0, 16), 1)
)
fitness$resting_hr <- round(clip(
  52 + 2.4 * fitness$weekly_exercise_hrs + rnorm(n, 0, 4.5),
  45, 110
))
write_data(fitness, "fitness")

# listings: bigger homes, lower prices ----------------------------------------
n <- 180
listings <- data.frame(
  listing_id = sample(10000:99999, n),
  neighborhood = sample(
    c("Eastmoor", "Riverside", "Old Town", "Hillcrest", "The Flats"),
    n,
    replace = TRUE
  ),
  year_built = sample(1925:2022, n, replace = TRUE),
  sqft = round(runif(n, 650, 3900))
)
listings$bedrooms <- clip(round(listings$sqft / 900 + rnorm(n, 0.8, 0.7)), 1, 6)
listings$price <- round(clip(
  980000 - 160 * listings$sqft + rnorm(n, 0, 48000),
  120000, 1.2e6
), -2)
write_data(listings, "listings")

# dosage: higher dose, worse symptoms -----------------------------------------
n <- 140
dosage <- data.frame(
  subject = sprintf("S-%03d", seq_len(n)),
  site = sample(paste0("Site ", LETTERS[1:4]), n, replace = TRUE),
  age = round(clip(rnorm(n, 47, 14), 18, 80)),
  dose_mg = sample(c(0, 25, 50, 100, 150, 200), n, replace = TRUE)
)
dosage$symptom_score <- round(clip(
  18 + 0.22 * dosage$dose_mg + rnorm(n, 0, 7),
  0, 100
), 1)
write_data(dosage, "dosage")

# commute: longer distances, shorter commutes ---------------------------------
n <- 150
commute <- data.frame(
  respondent = seq_len(n),
  borough = sample(
    c("North End", "Harborview", "Midtown", "Lakeside"),
    n,
    replace = TRUE
  ),
  vehicle = sample(
    c("car", "bus", "train", "bike"),
    n,
    replace = TRUE,
    prob = c(0.5, 0.2, 0.2, 0.1)
  ),
  distance_km = round(runif(n, 1, 42), 1)
)
commute$commute_min <- round(clip(
  58 - 1.05 * commute$distance_km + rnorm(n, 0, 5.5),
  4, 90
))
write_data(commute, "commute")

# subscribers: smiley face in tenure vs monthly spend -------------------------
n_bg <- 190
bg <- data.frame(
  tenure_months = round(runif(n_bg, 0, 72), 1),
  monthly_spend = NA_real_
)
bg$monthly_spend <- round(clip(
  35 + 0.55 * bg$tenure_months + rnorm(n_bg, 0, 16),
  5, 130
), 2)

eye <- function(cx, cy, n = 11) {
  data.frame(
    tenure_months = round(rnorm(n, cx, 0.9), 1),
    monthly_spend = round(rnorm(n, cy, 1.6), 2)
  )
}
t_arc <- seq(pi + 0.45, 2 * pi - 0.45, length.out = 38)
mouth <- data.frame(
  tenure_months = round(38 + 13 * cos(t_arc) + rnorm(38, 0, 0.35), 1),
  monthly_spend = round(72 + 26 * sin(t_arc) + rnorm(38, 0, 0.7), 2)
)
face <- rbind(eye(30, 88), eye(46, 88), mouth)

subscribers <- rbind(bg, face)
subscribers <- subscribers[sample(nrow(subscribers)), ]
subscribers$plan <- sample(
  c("basic", "standard", "premium"),
  nrow(subscribers),
  replace = TRUE,
  prob = c(0.45, 0.35, 0.2)
)
subscribers$signup_year <- sample(2019:2025, nrow(subscribers), replace = TRUE)
subscribers$customer_id <- sprintf("C%05d", sample(10000:99999, nrow(subscribers)))
subscribers <- subscribers[, c(
  "customer_id", "plan", "signup_year", "tenure_months", "monthly_spend"
)]
write_data(subscribers, "subscribers")

# sensors: heart shape in temperature vs humidity -----------------------------
n_bg <- 180
sensors_bg <- data.frame(
  temperature_c = round(runif(n_bg, 4, 36), 1),
  humidity_pct = NA_real_
)
sensors_bg$humidity_pct <- round(clip(
  85 - 1.1 * sensors_bg$temperature_c + rnorm(n_bg, 0, 11),
  5, 100
), 1)

t_heart <- seq(0, 2 * pi, length.out = 60)
heart_x <- 16 * sin(t_heart)^3
heart_y <- 13 * cos(t_heart) - 5 * cos(2 * t_heart) -
  2 * cos(3 * t_heart) - cos(4 * t_heart)
heart <- data.frame(
  temperature_c = round(21 + 0.45 * heart_x + rnorm(60, 0, 0.25), 1),
  humidity_pct = round(52 + 1.4 * heart_y + rnorm(60, 0, 0.6), 1)
)

sensors <- rbind(sensors_bg, heart)
sensors <- sensors[sample(nrow(sensors)), ]
sensors$station = sprintf("ST-%02d", sample(1:18, nrow(sensors), replace = TRUE))
sensors$battery_pct <- round(runif(nrow(sensors), 11, 100))
sensors <- sensors[, c("station", "battery_pct", "temperature_c", "humidity_pct")]
write_data(sensors, "sensors")

# trawl: tight off-curve cluster in length vs weight --------------------------
n_bg <- 200
trawl_bg <- data.frame(
  species = sample(
    c("haddock", "whiting", "pollock", "hake"),
    n_bg,
    replace = TRUE
  ),
  depth_m = round(runif(n_bg, 30, 220)),
  length_cm = round(runif(n_bg, 9, 78), 1)
)
trawl_bg$weight_g <- round(
  0.0095 * trawl_bg$length_cm^3 * exp(rnorm(n_bg, 0, 0.16))
)

blob_n <- 24
blob <- data.frame(
  species = sample(c("haddock", "whiting"), blob_n, replace = TRUE),
  depth_m = round(runif(blob_n, 30, 220)),
  length_cm = round(rnorm(blob_n, 29, 1.1), 1),
  weight_g = round(rnorm(blob_n, 2550, 70))
)

trawl <- rbind(trawl_bg, blob)
trawl <- trawl[sample(nrow(trawl)), ]
trawl$haul <- sample(1:12, nrow(trawl), replace = TRUE)
trawl <- trawl[, c("haul", "species", "depth_m", "length_cm", "weight_g")]
write_data(trawl, "trawl")

# orchard: five-pointed star outline in tree age vs yield ---------------------
n_bg <- 210
orchard_bg <- data.frame(
  variety = sample(
    c("honeycrisp", "gala", "fuji", "braeburn"),
    n_bg,
    replace = TRUE
  ),
  soil_ph = round(runif(n_bg, 5.4, 7.3), 1),
  tree_age_yrs = round(runif(n_bg, 2, 40), 1)
)
orchard_bg$yield_kg <- round(clip(
  8 + 4.2 * orchard_bg$tree_age_yrs - 0.052 * orchard_bg$tree_age_yrs^2 +
    rnorm(n_bg, 0, 9),
  0, 130
), 1)

star_vertices <- function(cx, cy, rx_outer, ry_outer, points = 5) {
  angles <- pi / 2 + seq(0, 2 * pi, length.out = 2 * points + 1)[-(2 * points + 1)]
  r <- rep(c(1, 0.42), points)
  data.frame(
    x = cx + rx_outer * r * cos(angles),
    y = cy + ry_outer * r * sin(angles)
  )
}
v <- star_vertices(14, 100, 6.5, 24)
v <- rbind(v, v[1, ])
star_pts <- do.call(rbind, lapply(seq_len(nrow(v) - 1), function(i) {
  s <- seq(0, 1, length.out = 6)[-6]
  data.frame(
    x = v$x[i] + s * (v$x[i + 1] - v$x[i]),
    y = v$y[i] + s * (v$y[i + 1] - v$y[i])
  )
}))
star <- data.frame(
  variety = sample(c("honeycrisp", "gala"), nrow(star_pts), replace = TRUE),
  soil_ph = round(runif(nrow(star_pts), 5.4, 7.3), 1),
  tree_age_yrs = round(star_pts$x + rnorm(nrow(star_pts), 0, 0.12), 1),
  yield_kg = round(star_pts$y + rnorm(nrow(star_pts), 0, 0.5), 1)
)

orchard <- rbind(orchard_bg, star)
orchard <- orchard[sample(nrow(orchard)), ]
orchard$block <- sample(paste0("B", 1:8), nrow(orchard), replace = TRUE)
orchard <- orchard[, c("block", "variety", "soil_ph", "tree_age_yrs", "yield_kg")]
write_data(orchard, "orchard")

# retention: perfect lattice in days active vs sessions -----------------------
n_bg <- 230
retention_bg <- data.frame(
  platform = sample(
    c("ios", "android", "web"),
    n_bg,
    replace = TRUE,
    prob = c(0.4, 0.38, 0.22)
  ),
  signup_month = sample(month.abb, n_bg, replace = TRUE),
  days_active = round(runif(n_bg, 1, 365))
)
retention_bg$sessions <- round(clip(
  0.62 * retention_bg$days_active + rnorm(n_bg, 0, 32),
  1, 400
))

lattice <- expand.grid(
  days_active = seq(235, 285, by = 10),
  sessions = seq(310, 360, by = 10)
)
lattice$platform <- sample(
  c("ios", "android", "web"),
  nrow(lattice),
  replace = TRUE
)
lattice$signup_month <- sample(month.abb, nrow(lattice), replace = TRUE)
lattice <- lattice[, c("platform", "signup_month", "days_active", "sessions")]

retention <- rbind(retention_bg, lattice)
retention <- retention[sample(nrow(retention)), ]
retention$user_id <- sprintf("U%06d", sample(1e5:(1e6 - 1), nrow(retention)))
retention <- retention[, c(
  "user_id", "platform", "signup_month", "days_active", "sessions"
)]
write_data(retention, "retention")
