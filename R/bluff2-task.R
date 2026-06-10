#' The bluffbench2 task
#'
#' Combines [bluff2_dataset], [bluff2_solver], and [bluff2_scorer] into a
#' [vitals::Task] for evaluating language models' willingness to pause and
#' examine artifacts that surface in plots during routine data analysis.
#'
#' @param epochs Number of evaluation epochs to run. Default is 1.
#' @param dir Directory for logging evaluation results. Default is
#'   `vitals::vitals_log_dir()`.
#' @param samples Row indices from `bluff2_dataset` to evaluate. Default is
#'   all rows.
#'
#' @return A [vitals::Task] object. Call `$eval(solver_chat = chat)` to run
#'   the evaluation, where `chat` is an ellmer Chat object. The scorer uses
#'   Claude Sonnet 4.6 by default; pass `scorer_chat` to override.
#'
#' @seealso [bluff2_dataset], [bluff2_solver], [bluff2_scorer], [vitals::Task]
#'
#' @examplesIf FALSE
#' chat <- ellmer::chat_anthropic()
#' tsk <- bluff2_task()
#' tsk$eval(solver_chat = chat)
#'
#' @export
bluff2_task <- function(
    epochs = 1,
    dir = vitals::vitals_log_dir(),
    samples = seq_len(nrow(bluff2_dataset))
) {
  vitals::Task$new(
    dataset = bluff2_dataset[samples, ],
    solver = bluff2_solver,
    scorer = bluff2_scorer,
    epochs = epochs,
    name = "bluffbench2",
    dir = dir
  )
}
