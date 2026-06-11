# Generates a realistic-but-random file name for a sample's data. Templates
# take the sample's `data_name` (e.g. "thymoma") as input but may or may not
# incorporate it, mimicking the variety of names data files have in the wild.
generate_file_name <- function(data_name, ext) {
  template <- sample(file_name_templates, 1)[[1]]
  paste0(template(data_name), ".", ext)
}

file_name_templates <- list(
  function(name) {
    year <- sample(2023:2026, 1)
    month <- sprintf("%02d", sample(1:12, 1))
    glue::glue("{name}-{year}-{month}")
  },
  function(name) {
    suffix <- sample(c("final", "clean", "export", "v2", "merged"), 1)
    sep <- sample(c("_", "-"), 1)
    glue::glue("{name}{sep}{suffix}")
  },
  function(name) {
    sample(c("data-final", "data_export", "dat", "data-raw", "full_data"), 1)
  },
  function(name) {
    prefix <- sample(c("analysis", "study", "project", "cohort"), 1)
    glue::glue("{prefix}-data")
  },
  function(name) {
    glue::glue("{toupper(substr(name, 1, 4))}_{sprintf('%03d', sample(999, 1))}")
  }
)

# Generates the name of the data frame object for `env` and `rda` delivery
# modes.
generate_object_name <- function(data_name) {
  variants <- c(
    data_name,
    paste0(data_name, "_", sample(c("df", "dat", "data"), 1)),
    "df",
    "dat"
  )
  sample(variants, 1, prob = c(0.4, 0.3, 0.15, 0.15))
}

# Generates a homey-looking absolute working directory for the conversation to
# be narrated as taking place in. The solver actually runs in a temp dir; this
# is only the path the model is told about (in the system prompt and the
# environment context), so a session reads like a real user's project rather
# than a sandbox under /var/folders. Tool inputs and outputs are translated
# between this path and the real temp dir so the illusion stays consistent
# (see `resolve_path()` and the path masking in the R and shell tools).
generate_display_wd <- function(data_name) {
  user <- sample(
    c("jordan", "alex", "sam", "priya", "wlee", "mchen", "rkaur", "dlopez"),
    1
  )
  area <- sample(
    c(
      "Documents", "projects", "work", "repos",
      "Documents/projects", "Documents/work", "Desktop"
    ),
    1
  )
  project <- sample(
    c(
      data_name,
      paste0(data_name, "-analysis"),
      paste0(data_name, "_study"),
      "analysis",
      "data-analysis",
      paste0("q", sample(1:4, 1), "-analysis"),
      "fieldwork",
      "scratch"
    ),
    1
  )
  file.path("/Users", user, area, project)
}
