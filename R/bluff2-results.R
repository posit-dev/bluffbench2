#' bluffbench2 results
#'
#' @description
#' The bluffbench2 results contain evaluation scores from running language
#' models on the bluffbench2 dataset. Each row represents one model's response
#' to one sample in one epoch, showing whether the model flagged the sample's
#' artifact on its own, only after a follow-up nudge, or never.
#'
#' The dataset is a tibble with one row per model-sample-epoch combination,
#' containing:
#' * `model`: The name of the language model evaluated, including the thinking
#'   mode in parentheses (e.g. "Claude Opus 4.8 (medium)").
#' * `id`: Unique identifier for the sample (matches `bluff2_dataset$id`).
#' * `epoch`: The evaluation epoch (each model-sample pair is evaluated across
#'   multiple epochs).
#' * `score`: An ordered factor `I < P < C`. `C` means the model flagged the
#'   artifact on its own, before the follow-up; `P` means it only described the
#'   artifact after the follow-up nudged it to look; `I` means it never
#'   described the artifact.
#' * `thinking`: Logical; `TRUE` for models run with medium thinking effort,
#'   `FALSE` for non-thinking variants.
#' * `cost`: Solver cost in USD for running this individual sample-epoch.
#'   Derived from the sample's token usage and per-model pricing. Summing `cost`
#'   within a model yields that model variant's total evaluation cost.
#' * `lab`: The organization that developed the model: "Anthropic", "OpenAI",
#'   or "Google".
#' * `release_date`: The date the model was publicly released, as a [Date].
#' * `release_date_source`: A URL citing the source for the release date.
#'
#' @format A tibble with columns `model`, `id`, `epoch`, `score`, `thinking`,
#'   `cost`, `lab`, `release_date`, and `release_date_source`.
"bluff2_results"

# See usage in data-raw/bluff2_results.R. Each run of the eval writes one
# timestamped json log into `inst/run/logs`; logs are keyed back to run names
# by the model slug in their filenames (`{timestamp}_bluffbench2-{model}-{hash}`),
# and the most recent log per run wins. Add an entry here for each new run.
run_names <- c(
  "claude-opus-4-8" = "opus_4_8_medium",
  "claude-fable-5" = "fable_5_medium",
  "claude-sonnet-5" = "sonnet_5_medium",
  "gpt-5.5" = "gpt_5_5_medium",
  "gpt-5.6-terra" = "gpt_5_6_terra_medium",
  "gpt-5.6-sol" = "gpt_5_6_sol_medium",
  "gemini-3.5-flash" = "gemini_3_5_flash_medium",
  "gemini-3.6-flash" = "gemini_3_6_flash_medium"
)

process_results <- function() {
  log_files <- sort(list.files(
    "inst/run/logs",
    pattern = "\\.json$",
    full.names = TRUE
  ))
  slugs <- gsub(
    "^[^_]+_bluffbench2-|-[0-9a-f]+\\.json$",
    "",
    basename(log_files)
  )
  latest <- tapply(log_files, slugs, function(files) files[length(files)])

  results <- purrr::imap(latest, function(file, slug) {
    res <- vitals::vitals_log_read(file)
    res$task <- run_names[[slug]]
    res
  })

  results <- purrr::list_rbind(unname(results))
  results$score <- factor(results$score, levels = c("I", "P", "C"), ordered = TRUE)
  results[c("task", "id", "epoch", "score", "solver_chat")]
}
