# Shell tool definitions modeled on a contemporary coding agent's tool set,
# with minimal working implementations. Each factory takes a `state`
# environment carrying per-sample context: `dir` (the sample's working
# directory) and `read_files` (paths Read so far, used to enforce
# read-before-edit).

new_tool_state <- function(dir, display_dir = dir) {
  state <- new.env(parent = emptyenv())
  state$dir <- dir
  state$display_dir <- display_dir
  state$read_files <- character()
  state
}

shell_tools <- function(state) {
  tools <- list(
    tool_bash(state),
    tool_read(state),
    tool_edit(state),
    tool_write(state),
    tool_glob(state),
    tool_grep(state),
    tool_agent(),
    tool_ask_user_question()
  )

  if (stats::runif(1) < 0.5) {
    tools <- c(tools, list(tool_web_fetch(), tool_web_search()))
  }

  tools
}

tool_result_error <- function(message) {
  ellmer::ContentToolResult(error = message)
}

# Maps a path the model supplies (which lives in the narrated `display_dir`
# world) onto the real temp dir where files actually are. Paths under
# `display_dir` keep their sub-path; any other absolute path is reduced to its
# basename so it still lands in the (flat) working directory rather than
# escaping the sandbox.
resolve_path <- function(path, state) {
  display <- state$display_dir %||% state$dir
  if (startsWith(path, display)) {
    rel <- sub("^/", "", substring(path, nchar(display) + 1L))
    return(if (nzchar(rel)) file.path(state$dir, rel) else state$dir)
  }
  if (startsWith(path, "/") || grepl("^[A-Za-z]:", path)) {
    return(file.path(state$dir, basename(path)))
  }
  file.path(state$dir, path)
}

# Rewrites the real temp dir to the narrated `display_dir` in text shown to
# the model (tool output), and the reverse for paths the model passes in (tool
# input). No-ops when the two coincide (i.e. when no fake path is in play).
mask_real_dir <- function(text, state) {
  mask_persona(text, state$dir, state$display_dir)
}

# Rewrites machine-specific details in tool output to match the narrated
# persona: the real working directory becomes `display_dir`, and the real OS
# username (as it surfaces in e.g. `ls -l` owner columns or `~` expansions)
# becomes the persona username embedded in `display_dir`. No-op when no fake
# path is in play. Shared by the shell tools and the R tool.
mask_persona <- function(text, real_dir, display_dir) {
  if (is.null(display_dir) || identical(real_dir, display_dir)) {
    return(text)
  }
  text <- gsub(real_dir, display_dir, text, fixed = TRUE)
  real_user <- Sys.info()[["user"]]
  persona <- display_user(display_dir)
  if (!is.na(persona) && nzchar(real_user) && !identical(real_user, persona)) {
    text <- gsub(real_user, persona, text, fixed = TRUE)
  }
  text
}

# The persona username is the first path component under /Users in a display
# working directory like /Users/priya/Documents/analysis (see
# generate_display_wd()).
display_user <- function(display_dir) {
  parts <- strsplit(display_dir, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) >= 2 && identical(parts[[1]], "Users")) {
    parts[[2]]
  } else {
    NA_character_
  }
}

unmask_display_dir <- function(text, state) {
  if (identical(state$dir, state$display_dir)) {
    return(text)
  }
  gsub(state$display_dir, state$dir, text, fixed = TRUE)
}

tool_bash <- function(state) {
  run_bash <- function(command, description = NULL, timeout = NULL, run_in_background = NULL) {
    command <- unmask_display_dir(command, state)
    out_file <- tempfile()
    status <- tryCatch(
      withr::with_dir(
        state$dir,
        suppressWarnings(system2(
          "/bin/zsh",
          c("-c", shQuote(command)),
          stdout = out_file,
          stderr = out_file
        ))
      ),
      error = function(e) e
    )
    if (inherits(status, "error")) {
      return(tool_result_error(mask_real_dir(conditionMessage(status), state)))
    }
    output <- readLines(out_file, warn = FALSE)
    text <- mask_real_dir(paste(output, collapse = "\n"), state)
    if (nchar(text) > 30000) {
      text <- paste0(substr(text, 1, 30000), "\n... [output truncated]")
    }
    if (!is.null(status) && status != 0) {
      return(tool_result_error(paste0(
        "Command exited with status ", status, "\n", text
      )))
    }
    if (identical(text, "")) "(no output)" else text
  }

  ellmer::tool(
    run_bash,
    name = "Bash",
    description = paste0(
      "Runs a bash command in the user's shell and returns its output.\n\n",
      "Commands share a persistent working directory across calls, though ",
      "other shell state (variables, aliases) does not persist. The shell ",
      "is initialized from the user's login profile.\n\n",
      "IMPORTANT: Do not use this tool to run `cat`, `head`, `tail`, `sed`, ",
      "`awk`, or `echo` unless you've confirmed that no dedicated tool can ",
      "do the job. Prefer the purpose-built tools instead, as they give ",
      "the user a better experience:\n\n",
      " - Reading files: Use Read (not cat/head/tail)\n",
      " - Editing files: Use Edit (not sed/awk)\n",
      " - Creating files: Use Write (not echo >/cat <<EOF)\n",
      " - Communicating: Output text directly (not echo/printf)"
    ),
    arguments = list(
      command = ellmer::type_string("The bash command to run"),
      description = ellmer::type_string(
        paste0(
          "A short, active-voice summary of what the command does. ",
          "Keep it brief (5-10 words) for everyday commands:\n",
          "- ls -> \"List files in current directory\"\n",
          "- git status -> \"Show working tree status\"\n\n",
          "Add more detail for commands that are harder to parse at a ",
          "glance (pipelines, uncommon flags, etc.)."
        ),
        required = FALSE
      ),
      timeout = ellmer::type_number(
        "Timeout in milliseconds (optional, max 600000)",
        required = FALSE
      ),
      run_in_background = ellmer::type_boolean(
        "Set to true to run this command in the background.",
        required = FALSE
      )
    )
  )
}

tool_read <- function(state) {
  run_read <- function(file_path, offset = NULL, limit = NULL, pages = NULL) {
    path <- resolve_path(file_path, state)
    if (!file.exists(path)) {
      return(tool_result_error(paste0("File does not exist: ", file_path)))
    }
    state$read_files <- union(state$read_files, path)

    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("png", "jpg", "jpeg", "gif", "webp")) {
      return(ellmer::content_image_file(path))
    }
    if (ext %in% c("rda", "rdata", "rds")) {
      return(tool_result_error(paste0(
        "Cannot display binary file as text: ", file_path
      )))
    }

    lines <- readLines(path, warn = FALSE)
    if (length(lines) == 0) {
      return("<system-reminder>This file exists but is empty.</system-reminder>")
    }
    start <- if (is.null(offset)) 1L else max(1L, as.integer(offset))
    n <- if (is.null(limit)) 2000L else as.integer(limit)
    end <- min(length(lines), start + n - 1L)
    if (start > length(lines)) {
      return(tool_result_error("Offset is beyond the end of the file."))
    }
    shown <- lines[start:end]
    numbered <- sprintf("%6d\t%s", seq(start, end), shown)
    paste(numbered, collapse = "\n")
  }

  ellmer::tool(
    run_read,
    name = "Read",
    description = paste0(
      "Reads a file from the local filesystem and returns its contents. ",
      "You can access any file on the machine with this tool.\n\n",
      "Usage:\n",
      "- By default reads up to 2000 lines from the start of the file\n",
      "- If you only need a specific section of a large file, use offset ",
      "and limit to read just that part.\n",
      "- Output is in cat -n format with line numbers beginning at 1\n",
      "- Image files (PNG, JPG, etc.) are returned visually since this is ",
      "a multimodal LLM.\n",
      "- This tool reads files only, not directories. Use the shell tool ",
      "to list directory contents.\n",
      "- If a file exists but is empty, you'll get a system reminder ",
      "warning instead of file contents."
    ),
    arguments = list(
      file_path = ellmer::type_string(
        "Absolute path to the file to read"
      ),
      offset = ellmer::type_integer(
        "Line number to begin reading from. Only needed when the file is too large to read in one call.",
        required = FALSE
      ),
      limit = ellmer::type_integer(
        "Number of lines to read. Only needed when the file is too large to read in one call.",
        required = FALSE
      ),
      pages = ellmer::type_string(
        "Page range for PDFs (e.g., \"1-5\"). Only applies to PDF files.",
        required = FALSE
      )
    )
  )
}

tool_edit <- function(state) {
  run_edit <- function(file_path, old_string, new_string, replace_all = FALSE) {
    path <- resolve_path(file_path, state)
    if (!file.exists(path)) {
      return(tool_result_error(paste0("File does not exist: ", file_path)))
    }
    if (!path %in% state$read_files) {
      return(tool_result_error(
        "You must Read the file before editing it."
      ))
    }
    content <- paste(readLines(path, warn = FALSE), collapse = "\n")
    n_matches <- lengths(gregexpr(old_string, content, fixed = TRUE))
    if (n_matches == 0 || !grepl(old_string, content, fixed = TRUE)) {
      return(tool_result_error("old_string was not found in the file."))
    }
    if (n_matches > 1 && !isTRUE(replace_all)) {
      return(tool_result_error(paste0(
        "old_string matches ", n_matches, " locations in the file. Include ",
        "more surrounding context to make the match unique, or set ",
        "replace_all to replace every occurrence."
      )))
    }
    if (isTRUE(replace_all)) {
      content <- gsub(old_string, new_string, content, fixed = TRUE)
    } else {
      content <- sub(old_string, new_string, content, fixed = TRUE)
    }
    writeLines(content, path)
    paste0("Successfully edited ", file_path)
  }

  ellmer::tool(
    run_edit,
    name = "Edit",
    description = paste0(
      "Makes exact string replacements in a file.\n\n",
      "Usage:\n",
      "- You must Read the file at least once earlier in the conversation ",
      "before editing it. Attempting an edit without a prior read will ",
      "fail.\n",
      "- Always prefer editing existing files over creating new ones.\n",
      "- The edit will FAIL when `old_string` matches more than one ",
      "location in the file. Include more surrounding context to make the ",
      "match unique, or set `replace_all` to replace every occurrence."
    ),
    arguments = list(
      file_path = ellmer::type_string(
        "Absolute path to the file to modify"
      ),
      old_string = ellmer::type_string("The exact text to find and replace"),
      new_string = ellmer::type_string(
        "The replacement text (must differ from old_string)"
      ),
      replace_all = ellmer::type_boolean(
        "Whether to replace all occurrences of old_string (default false)",
        required = FALSE
      )
    )
  )
}

tool_write <- function(state) {
  run_write <- function(file_path, content) {
    path <- resolve_path(file_path, state)
    if (file.exists(path) && !path %in% state$read_files) {
      return(tool_result_error(
        "File already exists. You must Read it before overwriting it."
      ))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(content, path)
    state$read_files <- union(state$read_files, path)
    paste0("Successfully wrote ", file_path)
  }

  ellmer::tool(
    run_write,
    name = "Write",
    description = paste0(
      "Creates or overwrites a file on the local filesystem.\n\n",
      "Usage:\n",
      "- If the file already exists, its contents will be overwritten.\n",
      "- For existing files, you MUST Read them first. The tool will fail ",
      "if you haven't read the file in this conversation.\n",
      "- For modifications to existing files, prefer Edit. Use Write for ",
      "new files or complete rewrites.\n",
      "- Do not create documentation files (*.md) or READMEs unless the ",
      "user explicitly asks."
    ),
    arguments = list(
      file_path = ellmer::type_string(
        "Absolute path for the file to write (must not be relative)"
      ),
      content = ellmer::type_string("The full content to write to the file")
    )
  )
}

tool_glob <- function(state) {
  run_glob <- function(pattern, path = NULL) {
    root <- if (is.null(path)) state$dir else resolve_path(path, state)
    files <- list.files(root, recursive = TRUE)
    matches <- files[grepl(glob_to_regex(pattern), files, perl = TRUE)]
    if (length(matches) == 0) {
      return("No files found.")
    }
    matches <- file.path(root, matches)
    info <- file.info(matches)
    matches <- matches[order(info$mtime, decreasing = TRUE)]
    mask_real_dir(paste(matches, collapse = "\n"), state)
  }

  ellmer::tool(
    run_glob,
    name = "Glob",
    description = paste0(
      "Finds files whose paths match a glob pattern. Results are returned ",
      "sorted by modification time, most recent first. Useful for locating ",
      "files by name pattern across a directory tree."
    ),
    arguments = list(
      pattern = ellmer::type_string(
        "Glob pattern to match file paths against"
      ),
      path = ellmer::type_string(
        "Directory to search in. Defaults to the current working directory.",
        required = FALSE
      )
    )
  )
}

# Converts a glob pattern (supporting `**` globstar) into a regular
# expression matched against paths relative to the search root.
glob_to_regex <- function(pattern) {
  rx <- gsub("([.\\\\+^$()\\[\\]{}|])", "\\\\\\1", pattern, perl = TRUE)
  rx <- gsub("\\*\\*/", "", rx)
  rx <- gsub("\\*\\*", "", rx)
  rx <- gsub("\\*", "[^/]*", rx)
  rx <- gsub("\\?", "[^/]", rx)
  rx <- gsub("", "(?:.*/)?", rx)
  rx <- gsub("", ".*", rx)
  paste0("^", rx, "$")
}

tool_grep <- function(state) {
  run_grep <- function(pattern, path = NULL, include = NULL, glob = NULL,
                       output_mode = NULL, head_limit = NULL, context = NULL) {
    root <- if (is.null(path)) state$dir else resolve_path(path, state)
    files <- list.files(root, full.names = TRUE, recursive = TRUE)
    file_filter <- include %||% glob
    if (!is.null(file_filter)) {
      keep <- Sys.glob(file.path(root, file_filter))
      files <- intersect(files, keep)
    }
    limit <- if (is.null(head_limit)) 100L else as.integer(head_limit)

    results <- character()
    for (f in files) {
      lines <- tryCatch(
        readLines(f, warn = FALSE),
        error = function(e) character()
      )
      hits <- which(grepl(pattern, lines, perl = TRUE))
      for (i in hits) {
        results <- c(results, paste0(f, ":", i, ":", lines[i]))
        if (length(results) >= limit) break
      }
      if (length(results) >= limit) break
    }
    if (length(results) == 0) {
      return("No matches found.")
    }
    mask_real_dir(paste(results, collapse = "\n"), state)
  }

  ellmer::tool(
    run_grep,
    name = "Grep",
    description = paste0(
      "Searches file contents for lines matching a regular expression. ",
      "Returns matching lines together with their file paths and line ",
      "numbers. Useful for locating specific text or patterns across a ",
      "codebase."
    ),
    arguments = list(
      pattern = ellmer::type_string("Regular expression to search for"),
      path = ellmer::type_string(
        "Directory to search in. Defaults to the current working directory.",
        required = FALSE
      ),
      include = ellmer::type_string(
        "Glob pattern to filter which files are searched",
        required = FALSE
      ),
      glob = ellmer::type_string(
        "Glob pattern to filter which files are searched",
        required = FALSE
      ),
      output_mode = ellmer::type_string(
        "Output mode: \"content\" to return full matching content",
        required = FALSE
      ),
      head_limit = ellmer::type_integer(
        "Maximum number of results to return",
        required = FALSE
      ),
      context = ellmer::type_integer(
        "Number of context lines to include around each match",
        required = FALSE
      )
    )
  )
}

tool_agent <- function() {
  ellmer::tool(
    function(prompt, description, subagent_type = NULL, model = NULL) {
      tool_result_error(
        "No subagent runtime is available in this session. Complete the task directly."
      )
    },
    name = "Agent",
    description = paste0(
      "Spawns a new sub-agent to handle a complex, multi-step task. Each ",
      "agent type comes with its own set of capabilities and tools.\n\n",
      "Set the subagent_type parameter to choose a specialized agent. If ",
      "omitted, a general-purpose agent is used.\n\n",
      "When launching multiple agents for independent work, include them ",
      "all in one message with multiple tool uses so they run in parallel."
    ),
    arguments = list(
      prompt = ellmer::type_string("The task for the sub-agent to carry out"),
      description = ellmer::type_string(
        "Brief (3-5 word) label for the task"
      ),
      subagent_type = ellmer::type_string(
        "Which specialized agent type to use",
        required = FALSE
      ),
      model = ellmer::type_string(
        "Optional model override for this agent",
        required = FALSE
      )
    )
  )
}

tool_ask_user_question <- function() {
  ellmer::tool(
    function(questions) {
      tool_result_error(
        "The user dismissed the question dialog without answering. Proceed with your best judgment."
      )
    },
    name = "AskUserQuestion",
    description = paste0(
      "Asks the user a question during execution. Use this to:\n",
      "1. Collect user preferences or requirements\n",
      "2. Clarify ambiguous instructions\n",
      "3. Get implementation decisions while working\n",
      "4. Present choices about which direction to take.\n\n",
      "Notes:\n",
      "- The user can always select \"Other\" to supply free-text input\n",
      "- Set multiSelect: true when the user should be able to pick more ",
      "than one option"
    ),
    arguments = list(
      questions = ellmer::type_string(
        "Questions to ask the user (1-4 questions), each with question text, header, options, and multiSelect flag."
      )
    )
  )
}

tool_web_fetch <- function() {
  ellmer::tool(
    function(url, format = NULL, prompt = NULL) {
      tool_result_error(
        "Network access is disabled in this environment."
      )
    },
    name = "WebFetch",
    description = paste0(
      "Downloads the content at a URL and returns it as text. Useful for ",
      "reading web pages, API endpoints, or online documentation."
    ),
    arguments = list(
      url = ellmer::type_string("URL to fetch content from"),
      format = ellmer::type_string(
        "Output format: \"text\" or \"markdown\"",
        required = FALSE
      ),
      prompt = ellmer::type_string(
        "Instructions for extracting specific content from the page",
        required = FALSE
      )
    )
  )
}

tool_web_search <- function() {
  ellmer::tool(
    function(query, domains = NULL) {
      tool_result_error(
        "Network access is disabled in this environment."
      )
    },
    name = "WebSearch",
    description = paste0(
      "Runs a web search and returns relevant results for the query."
    ),
    arguments = list(
      query = ellmer::type_string("The search query to run"),
      domains = ellmer::type_string(
        "Comma-separated list of domains to restrict results to",
        required = FALSE
      )
    )
  )
}
