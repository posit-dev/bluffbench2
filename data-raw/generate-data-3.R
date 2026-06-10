# Generates the third wave of synthetic datasets, replacing the embedded-shape
# samples with subtler artifacts screened to slip past models: a drifting
# sensor's smooth filament, sign-error-folded negatives, and a
# too-clean-to-be-true subset.

set.seed(20260617)

write_data <- function(df, name) {
  write.csv(
    df,
    file.path("inst", "data", paste0(name, ".csv")),
    row.names = FALSE
  )
}

clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# river: drifting turbidity sensor --------------------------------------------
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

drift_hours <- seq(95, 150, length.out = 45)
drift_vals <- 24 + cumsum(rnorm(45, 0.16, 0.18))
drift_vals <- drift_vals - (seq_along(drift_vals) / 45)^2 * (max(drift_vals) - 24)
drift <- data.frame(
  hour = round(drift_hours, 2),
  weekday = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[
    (floor(drift_hours / 24) %% 7) + 1
  ],
  battery_v = round(12.6 - 0.004 * drift_hours + rnorm(45, 0, 0.15), 2),
  turbidity_ntu = round(drift_vals, 2)
)
river <- rbind(river, drift)
river <- river[order(river$hour), ]
write_data(river, "river")

# stores: negative margins folded positive ------------------------------------
n <- 230
stores <- data.frame(
  store_id = sprintf("S%04d", sample(1000:9999, n)),
  region = sample(c("northeast", "southeast", "central", "west"), n, replace = TRUE),
  floor_area_sqm = round(runif(n, 40, 320), 1)
)
margin <- -8 + 0.075 * stores$floor_area_sqm + rnorm(n, 0, 9)
stores$weekly_margin_k <- round(abs(margin), 2)
write_data(stores, "stores")

# seals: one batch's measurements are suspiciously precise --------------------
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
clean$leak_rate_mlh <- round(2 + 0.31 * clean$pressure_psi + rnorm(45, 0, 0.25), 2)
seals <- rbind(seals, clean)
seals <- seals[sample(nrow(seals)), ]
write_data(seals, "seals")
