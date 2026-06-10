suppressMessages(library(ggplot2))
set.seed(20260622)

# meters v2: wider comb gaps (first 20 min of each hour missing), denser
n <- 780
minute_of_day <- runif(n, 0, 1440)
minute_of_day <- minute_of_day[(minute_of_day %% 60) > 20]
hr <- minute_of_day / 60
demand <- 2.2 + 1.4 * exp(-((hr - 8.5) / 2.4)^2) + 1.9 * exp(-((hr - 18.5) / 2.8)^2) +
  rnorm(length(hr), 0, 0.28)
meters <- data.frame(
  meter_group = sample(c("residential", "mixed"), length(hr), replace = TRUE),
  day = sample(c("2026-06-02", "2026-06-03", "2026-06-04"), length(hr), replace = TRUE),
  minute_of_day = round(minute_of_day, 1),
  demand_kw = round(demand, 3)
)
write.csv(meters, "inst/data/meters.csv", row.names = FALSE)

# deliveries v2: longer distances so the trend visibly runs into the 60-min
# ceiling and gets shaved
n <- 380
deliveries <- data.frame(
  courier = sample(c("bike", "scooter", "car"), n, replace = TRUE),
  zone = sample(paste0("Z", 1:5), n, replace = TRUE),
  distance_km = round(runif(n, 0.4, 12), 2)
)
deliveries$delivery_min <- round(8 + 4.4 * deliveries$distance_km +
  rexp(n, 1 / 7), 1)
deliveries <- deliveries[deliveries$delivery_min < 60, ]
write.csv(deliveries, "inst/data/deliveries.csv", row.names = FALSE)

p <- function(name, x, y) {
  df <- read.csv(file.path("inst/data", paste0(name, ".csv")))
  gg <- ggplot(df, aes(.data[[x]], .data[[y]])) + geom_point()
  ggsave(file.path("/tmp/bb2-check2", paste0(name, ".png")), gg, width = 8, height = 16/3, dpi = 96)
}
p("meters", "minute_of_day", "demand_kw")
p("deliveries", "distance_km", "delivery_min")
cat("done; deliveries n =", nrow(deliveries), "\n")
