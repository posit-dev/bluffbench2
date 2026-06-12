library(bluffbench2)

chat <- ellmer::chat_anthropic(
  model = "claude-opus-4-8",
  api_args = list(
    thinking = list(type = "adaptive"),
    output_config = list(effort = "medium")
  )
)

samples <- which(bluff2_dataset$id %in% c(
  "expenses_threshold_bunching",
  "outlets_copied_sales",
  "revenue_forward_filled",
  "traffic_timezone_echo"
))

tsk <- bluff2_task(epochs = 2, dir = "logs", samples = samples)
tsk$eval(solver_chat = chat, view = FALSE)

s <- tsk$get_samples()
writeLines(sprintf("%-28s %s", s$id, as.character(s$score)))
