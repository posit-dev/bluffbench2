devtools::load_all()

# The server-side-fallback beta returns a `content` block of type "fallback"
# (an audit marker naming the from/to models) that ellmer's Claude provider
# aborts on. It's safe to drop, so strip it before the original parser runs.
local({
  vt <- getNamespace("ellmer")$value_turn
  provider <- getNamespace("ellmer")$ProviderAnthropic
  orig <- S7::method(vt, provider)
  S7::method(vt, provider) <- function(provider, result, has_type = FALSE) {
    if (!is.null(result$content)) {
      keep <- vapply(
        result$content,
        function(b) is.null(b$type) || !identical(b$type, "fallback"),
        logical(1)
      )
      result$content <- result$content[keep]
    }
    orig(provider, result, has_type)
  }
})

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
run("fable_5_medium", anthropic_adaptive("claude-fable-5", fallback = "claude-opus-4-8"))
run("sonnet_5_medium", anthropic_adaptive("claude-sonnet-5"))
run("gpt_5_5_medium", openai_adaptive("gpt-5.5"))
run("gpt_5_6_terra_medium", openai_adaptive("gpt-5.6-terra"))
run("gpt_5_6_sol_medium", openai_adaptive("gpt-5.6-sol"))
run("gemini_3_5_flash_medium", gemini_adaptive("gemini-3.5-flash"))
run("gemini_3_6_flash_medium", gemini_adaptive("gemini-3.6-flash"))
