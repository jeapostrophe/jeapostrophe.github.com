---
title: "You're Using 40% of Your AI"
date: 2026-03-06
type: article
description: "The context window isn't a bucket you fill up. It's a budget you have to allocate."
---

## The Budget

Claude has a 200,000-token context window. You're using less than half of it. You might think of the window as a bucket you fill up. It's actually a budget you have to allocate.

When you start a task, the window gets carved up. These aren't exact numbers, but they're a useful way to think about where your window goes: 16% on conversation history, another 27% on understanding the problem — scanning files, reading code, building a mental model. Then 16% is reserved for auto-compression. That leaves 40% for doing the work.

That compression buffer exists because the context window is absolute — input and output share the same space. When the window fills up, Claude summarizes everything and starts fresh. Without the buffer, there would be no tokens left to produce a summary. With it, the summary is still lossy: 84% of your context condensed into 16%, and the details that drop out may be exactly the ones the next step needs.

![Context window budget allocation](budget.svg)

You're working with 40% of what you paid for.

And it's worse than that. Anyone who's worked with long contexts knows: more isn't better. Quality degrades before you hit the limit. It's not just about staying under the limit. It's about attention quality per token. Every token you spend on the wrong thing makes the remaining tokens less effective.

## The Loop

The core workflow is five phases: **Request, Research, Plan, Execute, Compress.**

You describe the task. Claude gathers information. It forms a plan. It does the work. Then, if the context is getting full, it compresses what it knows and continues.

Claude Code's planning mode automates one version of this. The history and understanding might eat 40% of the window. Planning mode distills them into a compact plan and clears the context. That plan takes 10%. Now 70% of the window is available for execution. That's a 75% increase in usable space.

But planning mode is just one implementation of this idea. You can apply it yourself at every level. Before you even open Claude, you can pre-gather the context it needs — paste in the relevant files, link to the right documentation, specify which parts of the codebase matter. Every piece of context you provide directly is a piece Claude doesn't have to spend tokens discovering.

The Research phase matters because Claude knows about Rust, but it doesn't know about *your* Rust project — your architecture, your conventions, your constraints. Claude Code handles this for code by scanning your codebase before working. But you need to do the same for design decisions, product requirements, and domain knowledge that isn't in the code. Tell Claude to go get specific knowledge about the library, the API, the pattern it needs — before it starts working. If you skip this, Claude will work from vague priors and produce vague output.

## The Checkpoint

When a task touches many files, crosses multiple systems, or requires deep reasoning chains — assume it's bigger than it looks and break it up.

A fifteen-file refactor across three services needs 250k tokens of work in a 200k-token window. You don't know this upfront — and neither does Claude. It spends 180k tokens, hits the limit, compresses, and the output falls apart — because the compression threw away things the remaining work depended on.

Break that task into two: one that takes 160k and one that takes 120k. Yes, that's 280k total — more than the original 250k. It doesn't matter. Both tasks will succeed, where the single task would have failed. Redundancy is cheap; a failed task is expensive.

![Breaking a task into two smaller ones](checkpoint.svg)

If you can't estimate the cost, err toward smaller tasks. And when something breaks after compression, don't try to salvage it. Hit Esc twice to `/rewind`, split the plan, and start fresh. Claude makes code a commodity. You can throw away anything and try again. You aren't wasting anyone's afternoon. And if the task splits naturally, subagents give you separate context windows — parallelism and isolation for free. But you still have to choose what to split and why.

## The Bottleneck

You give Claude a task. It takes ten minutes. You switch to something else — another project, your email. Claude finishes. You come back.

Now *you've* lost context. What exactly did you ask for? What were the constraints? How were you going to check the output? What were you going to do next? You have a context window too, and yours doesn't come with a token counter. You won't notice it filling up until you're staring at Claude's output with no memory of what you were looking for.

As you get better at managing Claude's context, the bottleneck shifts to yours. The same discipline applies — write it down, keep it concise, make it retrievable. Before you switch away, externalize your state: what you asked, how you'll verify it, what comes after. A few lines in a notes file.

Both of you are finite. Both of you degrade when overloaded. Both of you work better with clean, focused, externalized state.

## The Job

Your job when working with Claude is not writing prompts. It's making four decisions:

1. **What work to do.** Which task, in what order, at what scope.
2. **What context to build** before doing the work. Which files to read, what to research, what background to provide.
3. **How to describe the work.** Clear enough that Claude doesn't waste tokens figuring out what you meant.
4. **How to verify the output.** What does good look like, and how will you check.

These are leadership skills: scoping, briefing, delegating, reviewing. The difference is that you're leading ideal workers. They care about the task. They don't have egos. There's no politics. You can throw away any work and restart without a meeting to soften the blow. The only variable is you.

The documentation you write along the way — the plans, the context files, the notes about how the system works — isn't overhead. It's what you need for the next ten tasks, and it's what you need to understand the system you're building.

This is the real problem with "AI slop." It's not that Claude does bad work. It's that people accept bad output because they haven't developed taste — they don't know what good looks like, or they don't bother to check. Taste is the quality function that makes context management worth doing. Without it, you'll efficiently produce mediocrity.
