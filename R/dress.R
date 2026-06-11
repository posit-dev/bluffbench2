# Real analysis data is rarely as clean as a freshly-simulated data frame:
# headers carry spaces and inconsistent case, categorical labels drift between
# spellings, a spreadsheet export leaves blank rows and a title band above the
# table or a second human-readable header row, missing values arrive spelled a
# dozen ways. `dress_data()` samples a few such blemishes per conversation so
# the import step takes real work rather than a single `read_csv()`, and so no
# two runs see the same mess.
#
# The structural csv mess (leading blank rows, a title band, a double header)
# touches no data values at all--it only adds rows around the table--so it is
# artifact-safe by construction.
#
# Two invariants keep the dressing from leaking (or destroying) the planted
# artifact:
#
#   1. No blemish is keyed to artifact-row membership. The artifact rows are
#      appended as a distinct block during data generation; if they formatted
#      differently from the rest, a model eyeballing the raw file would spot
#      them. So every blemish applies to a whole column, or randomly across all
#      rows independent of which block they came from.
#
#   2. Value-touching blemishes only ever operate on character columns. Every
#      artifact lives in the joint distribution of a numeric pair (and that
#      pair isn't always positional--river's is `hour` x `turbidity_ntu`, with
#      a numeric distractor between them). Confining label, whitespace, and
#      missingness mess to character columns guarantees we never round, re-noise,
#      or reveal an artifact value. Numeric columns get nothing but name changes.
dress_data <- function(df, mode) {
  if (stats::runif(1) < 0.7) {
    df <- dress_names(df)
  }
  if (stats::runif(1) < 0.5) {
    df <- dress_labels(df)
  }
  if (stats::runif(1) < 0.3) {
    df <- dress_stray_column(df)
  }

  write <- list(na = "NA", preamble = character(0), header_above = character(0))
  if (mode == "csv") {
    if (stats::runif(1) < 0.25) {
      write$preamble <- c(write$preamble, rep("", sample(1:3, 1)))
    }
    if (stats::runif(1) < 0.3) {
      write$preamble <- c(write$preamble, dress_title())
    }
    if (stats::runif(1) < 0.25) {
      write$header_above <- make_header_above(names(df))
    }
    if (anyNA(df) && stats::runif(1) < 0.6) {
      write$na <- sample(c("", "N/A", "-", "null"), 1)
    }
  }

  list(df = df, write = write)
}

write_dressed_csv <- function(df, path, write) {
  tc <- textConnection("body", "w", local = TRUE)
  utils::write.csv(df, tc, row.names = FALSE, na = write$na)
  close(tc)

  con <- file(path, "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(c(write$preamble, write$header_above, body), con)
}

# --- column names ------------------------------------------------------------

dress_names <- function(df) {
  names(df) <- mangle_names(names(df))
  df
}

# Renders the snake_case names in one of a few spreadsheet-ish styles, keeping
# the root word recognizable so a request phrased with the original name (or a
# colloquial version) still maps unambiguously to a column.
mangle_names <- function(nms) {
  scheme <- sample(c("title", "spaces", "dotted", "upper", "mixed"), 1)
  out <- switch(
    scheme,
    title = tools::toTitleCase(gsub("_", " ", nms)),
    spaces = gsub("_", " ", nms),
    dotted = gsub("_", ".", nms),
    upper = toupper(nms),
    mixed = vapply(nms, mangle_one_name, character(1), USE.NAMES = FALSE)
  )
  if (stats::runif(1) < 0.35) {
    i <- sample(length(out), 1)
    out[i] <- paste0(out[i], " ")
  }
  out
}

mangle_one_name <- function(nm) {
  sample(c(
    nm,
    gsub("_", " ", nm),
    gsub("_", ".", nm),
    tools::toTitleCase(gsub("_", " ", nm))
  ), 1)
}

# --- categorical labels ------------------------------------------------------

dress_labels <- function(df) {
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  if (length(char_cols) == 0) {
    return(df)
  }
  col <- sample(char_cols, 1)
  df[[col]] <- perturb_labels(df[[col]])
  if (stats::runif(1) < 0.4) {
    df[[col]][sample(nrow(df), sample(1:3, 1))] <- NA
  }
  df
}

# Drifts a label column's spelling the way hand-entered categoricals drift:
# some cells gain a stray case change, some a trailing space, most are left
# alone--so "north", "North", and "north " coexist.
perturb_labels <- function(x) {
  style <- sample(c("case", "whitespace", "both"), 1)
  vapply(
    x,
    function(v) {
      if (is.na(v)) {
        return(v)
      }
      if (style %in% c("case", "both") && stats::runif(1) < 0.3) {
        v <- if (stats::runif(1) < 0.5) upper_first(v) else toupper(v)
      }
      if (style %in% c("whitespace", "both") && stats::runif(1) < 0.2) {
        v <- paste0(v, " ")
      }
      v
    },
    character(1),
    USE.NAMES = FALSE
  )
}

upper_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# --- stray column ------------------------------------------------------------

dress_stray_column <- function(df) {
  switch(
    sample(c("index", "blank", "notes"), 1),
    index = cbind(X = seq_len(nrow(df)), df),
    blank = {
      df[[sample(c("notes", "comment", "flag"), 1)]] <- NA
      df
    },
    notes = {
      df[["notes"]] <- ""
      df
    }
  )
}

# --- csv structure -----------------------------------------------------------

# A short title band of the sort that lands above the header in spreadsheet
# exports, forcing the reader to skip past it.
dress_title <- function() {
  lines <- sample(c(
    "Export generated by reporting pipeline",
    "CONFIDENTIAL - internal use only",
    "Quarterly data extract",
    "Source: lab information system",
    "Sheet1"
  ), 1)
  if (stats::runif(1) < 0.5) {
    d <- as.Date("2025-02-01") + sample(0:540, 1)
    lines <- c(lines, paste0("exported ", format(d, "%Y-%m-%d")))
  }
  if (stats::runif(1) < 0.5) {
    lines <- c(lines, "")
  }
  lines
}

# A second, human-readable header row to sit above the machine column names,
# so the file opens with two stacked header rows and the reader has to decide
# which one names the data.
make_header_above <- function(nms) {
  labels <- tools::toTitleCase(gsub("[._]+", " ", trimws(nms)))
  paste(paste0('"', labels, '"'), collapse = ",")
}
