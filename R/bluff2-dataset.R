#' Bluffbench2 dataset
#'
#' @description
#' The bluffbench2 dataset contains samples for evaluating language models'
#' willingness to pause and examine artifacts that surface in plots during
#' routine data analysis. Each sample defines a multi-turn conversation where
#' the model loads a dataset, makes a couple of unremarkable plots, and then
#' makes a plot containing a planted artifact: a counterintuitive
#' relationship, an embedded shape, or a cluster of points that's clearly not
#' part of the data generating process.
#'
#' The dataset is a tibble with one row per sample, containing:
#' * `id`: Unique identifier for the sample.
#' * `input`: A list-column where each element is a tibble with:
#'   - `data_name`: A short name for the data (e.g. "thymoma") used to
#'     generate realistic-but-random file and object names at solve time.
#'   - `data_file`: Path to the data file, relative to
#'     `system.file(package = "bluffbench2")`.
#'   - `modes`: A character vector of allowed data delivery modes, a subset
#'     of `c("csv", "rda", "env")`. One is sampled per evaluation run.
#'   - `lull_turns`: A list of character vectors. Each character vector
#'     contains alternative phrasings (with `{a|b}` interpolation groups) for
#'     one unremarkable plotting request; one phrasing is chosen at random
#'     per run.
#'   - `artifact_turn`: A character vector of alternative phrasings for the
#'     request that produces the plot containing the artifact.
#' * `target`: Description of the artifact the model should notice and what
#'   an accurate description of it looks like.
#'
#' @format A tibble with columns `id`, `input`, and `target`.
#'
#' @examples
#' bluff2_dataset
#'
#' # View the input for the first sample
#' bluff2_dataset$input[[1]]
#'
"bluff2_dataset"
