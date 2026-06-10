# Questions before implementing

My working picture: plottwist's multi-turn, YAML-driven solver structure (temp
dir with a data file, randomized system prompt, turn phrasings sampled at
runtime), hollowharness's Claude Code-style tool definitions given minimal real
implementations, and bluffbench's artifact-driven LLM-judge scoring. A few
things I can't resolve from the sources:

## 1. How does the agent actually see the plot?

This is the load-bearing design decision. hollowharness's tools are shell-shaped
(Bash, Read, Edit, Write, ...). With real implementations, the natural flow for
plotting is: write an R script, `Rscript` it via Bash with `ggsave()` to a png,
then `Read` the png—which means the `Read` implementation must return real
image content (`ellmer::content_image_file()`) so the model can look at what it
made. But an agent isn't obligated to Read the png it just saved, and in real
Claude Code sessions often doesn't (the *user* looks at the plot).

The vision also mentions the data sometimes being "an R object already in the
environment," which implies an R-session tool (btw-style `run_r`, as in
plottwist's harness)—there, plots come back inline in the tool result and the
agent always sees them.

Options I see:

a. Shell-only harness (hollowharness tools + real impls); `Read` renders
   images; samples where the agent never views the png are scored I (or the
   prompts steer it toward viewing).
b. Hollowharness tools *plus* a run-R-code tool that returns plots inline
   (closer to Posit Assistant / databot).
c. Randomize between two harness flavors per sample: a Claude Code-ish shell
   harness and a Posit-Assistant-ish R-session harness. More realism/variance,
   more surface area to build and debug.

Which did you have in mind?

Answer: Add a run_r_code tool that's just btw's run_r. Provide slight
perturbations on the name and tool description of the tool.

## 2. Does the final user turn ask for interpretation, or just the plot?

In bluffbench, the prompt explicitly asks the model to describe what it sees.
The "willingness to pause and examine" framing suggests bluffbench2's artifact
turn might just say "plot x vs y" and test whether the agent *spontaneously*
flags the artifact. But then an agent that silently produces a perfect plot of
the artifact gets an I despite doing exactly what the user asked. Should the
artifact turn (sometimes? always?) include a "tell me what you see"-style ask,
or never, or should this be a solver parameter?

Answer: Do it plottwist-style, yeah--just have it make the plot first, and see
if it does anything / reacts at all. Also provide a follow-up turn, saying
"what do you see in the plot?"-ish. Ask the follow-up regardless, and then let
the grader decide whether it was needed or not.

## 3. Nudge turn and partial credit?

plottwist follows the artifact with a vague nudge ("fix the plot") and scores
C/P/I, where P = only caught it after the nudge. Should bluffbench2 do the same
(e.g., a final "anything look off?" turn, C/P/I), or end at the artifact turn
and score C/I like bluffbench?

Answer: Covered by the answer to 2--the "what do you see in the plot?"
follow-up is asked regardless, and the grader decides whether it was needed
(caught it unprompted vs. only after the follow-up).

## 4. Fixed data files, or regenerated per run?

Are the datasets generated once and shipped in `inst/` (plottwist-style: stable,
debuggable, but memorizable over time), or regenerated at solve time from the
sample's data-generating code with a random seed (point positions, n, maybe
filenames vary every run)? Same question for the random-ish filenames: one
fixed name per sample in the YAML, or drawn at runtime from a per-sample pool?
My lean is regenerate-at-solve-time with the DGP in the YAML, since
memorization resistance is an explicit goal, but it does make targets harder to
write precisely and runs noisier.

Answer: Fixed data files, but regenerate names at solve time. You might
imagine 5 "naming templates" that might take e.g. a name as input ("thymoma")
but then return a filename that may or may not take it into account.

## 5. Are well-known datasets in scope?

bluffbench's bread-and-butter samples mutate mtcars/iris/ChickWeight, leaning
on priors those datasets carry. With bluffbench2's random filenames, the analog
would be synthetic data with prior-activating *variable* names (horsepower-ish,
dosage-ish) rather than the literal datasets. Should literal well-known data
(e.g., mutated mtcars dumped to `engines-2024.csv`) appear at all, or should
every sample be novel synthetic data whose variable names do the prior
activation?

Answer: Every sample should be synthetic.
