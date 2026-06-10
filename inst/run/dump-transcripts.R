# Dumps a vitals eval log into one readable markdown transcript per sample,
# replacing inline images with placeholders, for review.
#
# Usage: Rscript inst/run/dump-transcripts.R <log.json> <out-dir>

args <- commandArgs(trailingOnly = TRUE)
log_path <- args[1]
out_dir <- args[2]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

log <- jsonlite::read_json(log_path)

content_text <- function(content) {
  if (is.null(content)) {
    return("")
  }
  if (is.character(content)) {
    return(content)
  }
  parts <- vapply(
    content,
    function(c) {
      if (!is.null(c$text)) {
        c$text
      } else if (identical(c$type, "image")) {
        "[image: a rendered plot was returned here]"
      } else {
        paste0("[", c$type %||% "content", "]")
      }
    },
    character(1)
  )
  paste(parts, collapse = "\n\n")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

for (s in log$samples) {
  lines <- c(
    paste0("# Sample: ", s$id),
    "",
    paste0("**Target:** ", s$target),
    ""
  )

  for (sc in s$scores) {
    lines <- c(
      lines,
      paste0("**Score:** ", sc$value %||% ""),
      "",
      paste0("**Scorer explanation:** ", sc$explanation %||% sc$answer %||% ""),
      ""
    )
  }
  if (!is.null(s$score)) {
    lines <- c(lines, paste0("**Score:** ", content_text(s$score$value)), "")
  }

  lines <- c(lines, "## Transcript", "")

  for (m in s$messages) {
    role <- m$role %||% "tool"
    lines <- c(lines, paste0("### ", toupper(role)), "")
    txt <- content_text(m$content)
    if (nzchar(txt)) {
      lines <- c(lines, txt, "")
    }
    if (!is.null(m$tool_calls)) {
      for (tc in m$tool_calls) {
        fn <- tc$`function` %||% tc$name %||% "?"
        arg_json <- tryCatch(
          jsonlite::toJSON(tc$arguments, auto_unbox = TRUE, pretty = TRUE),
          error = function(e) "<unserializable>"
        )
        lines <- c(
          lines,
          paste0("**Tool call:** `", fn, "`"),
          "",
          "```json",
          arg_json,
          "```",
          ""
        )
      }
    }
  }

  writeLines(lines, file.path(out_dir, paste0(s$id, "-", s$epoch %||% 1, ".md")))
}

cat("Wrote", length(log$samples), "transcripts to", out_dir, "\n")
