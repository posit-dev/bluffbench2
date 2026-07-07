withr::local_envvar(VITALS_LOG_DIR = "inst/run/logs")
devtools::load_all()

tsk <- bluff2_task(epochs = 2)

# Each provider exposes a medium-thinking (`*_adaptive`) and a reasoning-off
# (`*_nonthinking`) setting; the mechanism differs by provider:
#
# * Anthropic (Opus 4.6+): adaptive thinking + medium effort, or
#   `thinking = list(type = "disabled")`.
# * OpenAI: `reasoning_effort` of "medium" or "none".
# * Gemini 3.x: `thinkingLevel = "MEDIUM"`, or `thinkingBudget = 0`.

run <- function(name, solver_chat) {
  task <- tsk$clone()
  task$eval(solver_chat = solver_chat, view = FALSE)
  save(task, file = file.path("inst/run/tasks", paste0("tsk_", name, ".rda")))
}

anthropic_adaptive <- function(model, effort = "medium") {
  ellmer::chat_anthropic(
    model = model,
    api_args = list(
      thinking = list(type = "adaptive"),
      output_config = list(effort = effort)
    )
  )
}

anthropic_nonthinking <- function(model) {
  ellmer::chat_anthropic(
    model = model,
    api_args = list(thinking = list(type = "disabled"))
  )
}

openai_adaptive <- function(model, effort = "medium") {
  ellmer::chat_openai(
    model = model,
    params = ellmer::params(reasoning_effort = effort)
  )
}

openai_nonthinking <- function(model) {
  ellmer::chat_openai(
    model = model,
    params = ellmer::params(reasoning_effort = "none")
  )
}

gemini_adaptive <- function(model, effort = "medium") {
  ellmer::chat_google_gemini(
    model = model,
    api_args = list(
      generationConfig = list(
        thinkingConfig = list(thinkingLevel = toupper(effort))
      )
    )
  )
}

gemini_nonthinking <- function(model) {
  ellmer::chat_google_gemini(
    model = model,
    api_args = list(
      generationConfig = list(thinkingConfig = list(thinkingBudget = 0))
    )
  )
}

run("opus_4_8_medium", anthropic_adaptive("claude-opus-4-8"))
run("gpt_5_5_medium", openai_adaptive("gpt-5.5"))
run("gemini_3_5_flash_medium", gemini_adaptive("gemini-3.5-flash"))
