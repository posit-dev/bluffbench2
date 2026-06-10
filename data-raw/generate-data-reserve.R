# Reserve hard samples: generated like data-raw/generate-data-2.R but kept
# out of the dataset until calibration says they're needed.
set.seed(20260613)
clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)
write_data <- function(df, name) {
  write.csv(df, file.path("inst", "data", paste0(name, ".csv")), row.names = FALSE)
}

# ponds: dissolved oxygen probe stuck at one value -----------------------------
n <- 185
ponds <- data.frame(
  pond = sample(paste0("P", 1:8), n, replace = TRUE),
  fed_today = sample(c("yes", "no"), n, replace = TRUE),
  water_temp_c = round(runif(n, 12, 30), 1)
)
ponds$dissolved_o2_mgl <- round(clip(13.5 - 0.22 * ponds$water_temp_c +
  rnorm(n, 0, 0.7), 4, 14), 2)
stuck <- data.frame(
  pond = sample(paste0("P", 1:8), 28, replace = TRUE),
  fed_today = sample(c("yes", "no"), 28, replace = TRUE),
  water_temp_c = round(runif(28, 13, 29), 1),
  dissolved_o2_mgl = 8.45
)
ponds <- rbind(ponds, stuck)
ponds <- ponds[sample(nrow(ponds)), ]
write_data(ponds, "ponds")

# energy: regression-imputed heating usage -------------------------------------
n <- 155
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

# bridges: the same few bridges inspected repeatedly ---------------------------
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
