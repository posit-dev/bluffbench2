# Assembles a randomized system prompt from sections shipped with the package.
# The preamble section always comes first; the remaining sections appear in
# random order, with one variant sampled per section. `<wd>` placeholders are
# replaced with the sample's working directory so the model can resolve
# absolute paths.
random_system_prompt <- function(dir) {
  prompts_dir <- system.file("prompts/random-system", package = "bluffbench2")
  sections <- list.dirs(prompts_dir, full.names = TRUE, recursive = FALSE)

  sections <- c(sections[1], sample(sections[-1]))

  paragraphs <- vapply(
    sections,
    function(section) {
      files <- list.files(section, full.names = TRUE, pattern = "\\.md$")
      chosen <- sample(files, 1)
      paste(readLines(chosen, warn = FALSE), collapse = "\n")
    },
    character(1)
  )

  gsub("<wd>", dir, paste(paragraphs, collapse = "\n\n"), fixed = TRUE)
}
