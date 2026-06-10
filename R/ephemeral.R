# With some probability, returns a <system-reminder> block of unrelated
# context to prepend to a user message, mimicking the ephemeral context that
# agent harnesses interleave into conversations. Returns NULL otherwise.
#
# When the sampled context references files (e.g. an open notes.md), those
# files are written to `dir` so that a model that goes looking finds them.
maybe_ephemeral_context <- function(p = 0.5, dir = NULL) {
  if (stats::runif(1) > p) {
    return(NULL)
  }

  pool_dir <- system.file("prompts/ephemeral", package = "bluffbench2")
  files <- list.files(pool_dir, full.names = TRUE, pattern = "\\.md$")
  chosen <- sample(files, 1)
  context <- paste(readLines(chosen, warn = FALSE), collapse = "\n")

  if (!is.null(dir) && grepl("notes.md", context, fixed = TRUE)) {
    writeLines(
      c(
        "# notes",
        "",
        "- follow up with J. about the Q3 readout",
        "- renew cluster credentials before Friday",
        "- draft outline for the methods section"
      ),
      file.path(dir, "notes.md")
    )
  }

  paste0("<system-reminder>\n", context, "\n</system-reminder>")
}
