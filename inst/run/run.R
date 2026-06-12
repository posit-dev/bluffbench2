library(bluffbench2)

chat <- ellmer::chat_anthropic(
  model = "claude-opus-4-8",
  api_args = list(
    thinking = list(type = "adaptive"),
    output_config = list(effort = "medium")
  )
)

tsk <- bluff2_task(epochs = 2, dir = "logs")
tsk$eval(solver_chat = chat, view = FALSE)

samples <- tsk$get_samples()
print(table(samples$score))
print(data.frame(id = samples$id, score = as.character(samples$score)))

# Validate the log against Inspect's pydantic models so that the log viewer
# can render it.
log_file <- file.path("logs", rev(list.files("logs", pattern = "\\.json$"))[1])
validation <- system2(
  path.expand("~/.virtualenvs/vitals-venv/bin/python"),
  c(system.file("test/validate_log.py", package = "vitals"), log_file),
  stdout = TRUE,
  stderr = TRUE
)
cat(validation, sep = "\n")
