# Expands templates containing brace-delimited alternatives by sampling one
# alternative per group. e.g. "{Would|Could} you {plot|chart} this{?|}" might
# become "Could you plot this". Empty alternatives are allowed.
interpolate <- function(template) {
  while (grepl("\\{[^{}]*\\}", template)) {
    match <- regmatches(template, regexpr("\\{[^{}]*\\}", template))
    inner <- substr(match, 2, nchar(match) - 1)
    alternatives <- strsplit(inner, "|", fixed = TRUE)[[1]]
    if (endsWith(inner, "|") || length(alternatives) == 0) {
      alternatives <- c(alternatives, "")
    }
    chosen <- sample(alternatives, 1)
    template <- sub("\\{[^{}]*\\}", chosen, template)
  }
  template
}

# Samples one variant from a character vector of templates, then expands its
# brace alternatives.
sample_phrasing <- function(variants) {
  interpolate(sample(variants, 1))
}
