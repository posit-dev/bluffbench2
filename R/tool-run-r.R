# The run-R-code tool: btw's run R tool evaluated in a per-sample environment,
# with the name and description lightly perturbed per sample to resist
# memorization.
tool_run_r <- function(env, real_dir = NULL, display_dir = NULL) {
  ellmer::tool(
    function(code) btw_run_r(code, env, real_dir, display_dir),
    name = sample(run_r_names, 1),
    description = sample(run_r_descriptions, 1),
    arguments = list(
      code = ellmer::type_string("The R code to run")
    )
  )
}

# The solver runs in a temp dir but tells the model it's in a homey-looking
# `display_dir` (see generate_display_wd()). Translate any reference to that
# path in the code onto the real dir before running, and rewrite the real dir
# back to it in the captured output (e.g. getwd(), normalizePath()).
btw_run_r <- function(code, env, real_dir = NULL, display_dir = NULL) {
  fake_wd <- !is.null(display_dir) && !identical(real_dir, display_dir)
  if (fake_wd) {
    code <- gsub(display_dir, real_dir, code, fixed = TRUE)
  }
  result <- scrub_carriage_returns(btw:::btw_tool_run_r_impl(code, .envir = env))
  if (fake_wd) {
    result <- mask_run_r_paths(result, real_dir, display_dir)
  }
  result
}

mask_run_r_paths <- function(result, real_dir, display_dir) {
  rewrite <- function(x) mask_persona(x, real_dir, display_dir)
  value <- result@value
  if (is.character(value)) {
    result@value <- rewrite(value)
  } else if (is.list(value)) {
    result@value <- lapply(value, function(item) {
      if (inherits(item, "btw::ContentOutput")) {
        item@text <- rewrite(item@text)
      }
      item
    })
  }
  result
}

# Captured tool output can contain carriage-return-mediated redraws of the
# eval harness's own progress indicators (e.g. cli progress steps re-rendered
# when model-run code emits messages). Resolve \r the way a terminal would,
# so only the final content of each line survives.
scrub_carriage_returns <- function(result) {
  value <- result@value
  if (is.character(value)) {
    result@value <- resolve_cr(value)
  } else if (is.list(value)) {
    result@value <- lapply(value, function(item) {
      if (inherits(item, "btw::ContentOutput")) {
        item@text <- resolve_cr(item@text)
      }
      item
    })
  }
  result
}

resolve_cr <- function(text) {
  vapply(
    text,
    function(x) {
      x <- gsub("\r\n", "\n", x, fixed = TRUE)
      x <- gsub("[^\n\r]*\r", "", x, perl = TRUE)
      x <- gsub("(^|\n)ℹ (Solving|Scoring)(?=\n|$)", "\\1", x, perl = TRUE)
      gsub("\n{3,}", "\n\n", x)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

run_r_names <- c("run_r", "run_r_code", "execute_r", "execute_r_code")

run_r_descriptions <- c(
  paste0(
    "Run R code in the user's R session.\n\n",
    "Executes R code and captures printed values, text output, plots, ",
    "messages, warnings, and errors.\n\n",
    "Guidelines:\n",
    "- Work incrementally: each call should do one small, well-defined task\n",
    "- Create no more than one rendered figure per call; use separate calls ",
    "for multiple figures\n",
    "- Do not use this tool to talk to the user; explanations belong in the ",
    "assistant message\n",
    "- Code runs in the user's live R session and objects persist across ",
    "calls; rely on that default session and do not pass an explicit ",
    "`envir` argument or reference globalenv() or .GlobalEnv.\n",
    "- Write code that is safe, reversible, and non-destructive\n",
    "- Prefer clear, idiomatic R code, using tidyverse packages when available\n",
    "- Return results implicitly (`x`, not `print(x)`), making the last ",
    "expression the object you want to show"
  ),
  paste0(
    "Execute R code in the active R session and return the results.\n\n",
    "Output, messages, warnings, errors, and rendered plots are captured ",
    "and returned.\n\n",
    "Rules:\n",
    "- Keep each call small and focused on a single step\n",
    "- At most one figure per call\n",
    "- Read error messages carefully; after two failed attempts to fix an ",
    "error, stop and summarize what you tried\n",
    "- Code runs in the active session and variables persist between calls. ",
    "Use the default session rather than an explicit `envir`/globalenv()/.GlobalEnv; avoid ",
    "destructive side effects (file writes, package installation, shell ",
    "execution) unless the user explicitly asks\n",
    "- End each call with the expression whose value you want to see"
  ),
  paste0(
    "Run R code.\n\n",
    "Executes the provided code in the user's R session, capturing console ",
    "output, conditions, and plots.\n\n",
    "Usage notes:\n",
    "- Work in small, incremental steps rather than long scripts\n",
    "- One rendered figure per call, maximum\n",
    "- Code is evaluated in the user's R session and variables persist ",
    "between calls; don't manage environments yourself with an explicit ",
    "`envir`--just use the default and refrain from using .GlobalEnv or globalenv().\n",
    "- Avoid `print()` and `cat()`; return the object of interest as the ",
    "last expression\n",
    "- Write safe, non-destructive code, and prefer tidyverse idioms where ",
    "natural"
  )
)
