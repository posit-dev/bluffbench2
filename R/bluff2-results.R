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
#' * `cost`: Total solver cost in USD for running the full evaluation
#'   (all samples, all epochs) for this model variant. Derived from token usage
#'   and per-model pricing. The same value is repeated across all rows for a
#'   given model.
#'
#' @format A tibble with columns `model`, `id`, `epoch`, `score`, `thinking`,
#'   and `cost`.
"bluff2_results"

# See usage in data-raw/bluff2_results.R
process_results <- function() {
  task_files <- list.files("inst/run/tasks", full.names = TRUE)

  load_object <- function(file) {
    tmp <- new.env()
    load(file = file, envir = tmp)
    tmp[[ls(tmp)[1]]]
  }

  tasks <- list()
  for (task in task_files) {
    tasks[[gsub(".rda", "", basename(task))]] <- load_object(task)
  }
  names(tasks) <- gsub("tsk_", "", names(tasks))

  vitals::vitals_bind(!!!tasks)
}
