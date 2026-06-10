# Generates the claims dataset: a bad join repeats a few exact incomes, each
# with many different claim amounts (vertical strings in the scatter).

set.seed(20260621)
clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)

n <- 180
claims <- data.frame(
  claim_type = sample(c("auto", "home", "umbrella"), n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
  region = sample(c("north", "south", "coastal"), n, replace = TRUE),
  claimant_income_k = round(runif(n, 28, 140), 2)
)
claims$claim_amount_k <- round(clip(0.9 + 0.012 * claims$claimant_income_k + rnorm(n, 0, 0.55), 0.2, 4), 2)
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
write.csv(claims, "inst/data/claims.csv", row.names = FALSE)
