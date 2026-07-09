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
  withr::local_envvar(VITALS_LOG_DIR = "inst/run/logs")
  task <- tsk$clone()
  task$eval(solver_chat = solver_chat, view = FALSE)
}

# Fable/Mythos decline some requests with `stop_reason = "refusal"` rather than
# an error; the Anthropic products fall back to Opus 4.8 on these by default,
# but a direct API call does not. `fallback` opts into the server-side fallback
# so those refusals route to Opus instead of surfacing as empty turns the eval
# would score as failures to notice the artifact.
anthropic_adaptive <- function(model, effort = "medium", fallback = NULL) {
  api_args <- list(
    thinking = list(type = "adaptive"),
    output_config = list(effort = effort)
  )
  beta_headers <- character()
  if (!is.null(fallback)) {
    api_args$fallbacks <- list(list(model = fallback))
    beta_headers <- "server-side-fallback-2026-06-01"
  }
  ellmer::chat_anthropic(
    model = model,
    api_args = api_args,
    beta_headers = beta_headers
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
run("sonnet_5_medium", anthropic_adaptive("claude-sonnet-5"))
run("gpt_5_5_medium", openai_adaptive("gpt-5.5"))
run("gemini_3_5_flash_medium", gemini_adaptive("gemini-3.5-flash"))
