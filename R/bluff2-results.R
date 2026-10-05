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
  "claude-opus-5" = "opus_5_medium",
  "claude-fable-5-1" = "fable_5_1_medium",
  "claude-opus-5-5" = "opus_5_5_medium",
  "claude-sonnet-5-5" = "sonnet_5_5_medium",
  "gpt-5.5" = "gpt_5_5_medium",
  "gpt-5.6-terra" = "gpt_5_6_terra_medium",
  "gpt-5.6-sol" = "gpt_5_6_sol_medium",
  "gpt-6-astra" = "gpt_6_astra_medium",
  "gpt-6-sol" = "gpt_6_sol_medium",
  "gpt-6-luna" = "gpt_6_luna_medium",
  "gpt-6.1-sol" = "gpt_6_1_sol_medium",
  "gemini-3.5-flash" = "gemini_3_5_flash_medium",
  "gemini-3.6-flash" = "gemini_3_6_flash_medium",
  "gemini-3.7-flash" = "gemini_3_7_flash_medium",
  "gemini-3.8-flash" = "gemini_3_8_flash_medium"
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
    raw <- jsonlite::read_json(file, simplifyVector = FALSE)
    res$solver_cache_write_tokens <- vapply(raw$samples, function(sample) {
      model <- grep(
        paste0("/", slug, "$"),
        names(sample$model_usage),
        value = TRUE
      )
      if (length(model) == 0) {
        return(0)
      }
      sample$model_usage[[model[[1]]]]$input_tokens_cache_write
    }, numeric(1))
    res$task <- run_names[[slug]]
    res
  })

  results <- purrr::list_rbind(unname(results))
  results$score <- factor(results$score, levels = c("I", "P", "C"), ordered = TRUE)
  results[c(
    "task",
    "id",
    "epoch",
    "score",
    "solver_chat",
    "solver_cache_write_tokens"
  )]
}
