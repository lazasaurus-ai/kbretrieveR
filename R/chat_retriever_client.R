#' Stateless KB+Chat client using ellmer and Bedrock
#'
#' @param kb_id Knowledge Base ID
#' @param ellmer_chat An ellmer chat client object (e.g., from ellmer::chat_aws_bedrock())
#' @param profile AWS profile, optional
#' @param region AWS region, optional
#' @param retrieval_cfg Retrieval config (e.g., list(topK = 6))
#' @param system_prompt Optional system prompt to prepend
#' @param max_context_chars Max KB context length
#' @export
chat_retriever_client <- function(kb_id,
                                  ellmer_chat,
                                  profile = NULL,
                                  region = NULL,
                                  retrieval_cfg = list(topK = 6),
                                  system_prompt = NULL,
                                  max_context_chars = 12000) {

  trim_ctx <- function(x, n) {
    if (nchar(x) > n) substr(x, 1, n) else x
  }

  build_prompt <- function(question, refs) {
    ctx <- paste(refs$chunk_text, collapse = "\n---\n")
    ctx <- trim_ctx(ctx, max_context_chars)

    paste0(
      if (!is.null(system_prompt)) paste0("[System]\n", system_prompt, "\n\n") else "",
      "Use ONLY the following knowledge base context.\n",
      "If the answer isn't present, say you don't know.\n\n",
      "[KB Context]\n", ctx, "\n\n",
      "[User Question]\n", question
    )
  }

  ask <- function(question, echo = FALSE) {
    refs <- kb_retrieve(
      question      = question,
      kb_id         = kb_id,
      profile       = profile,
      region        = region,
      retrieval_cfg = retrieval_cfg
    )

    prompt <- build_prompt(question, refs)
    if (echo) cat("\n--- PROMPT SENT TO MODEL ---\n", prompt, "\n\n")

    ans <- ellmer_chat$chat(prompt)

    tibble::tibble(
      answer    = as.character(ans$text %||% ans),
      citations = list(refs),
      raw       = list(ans)
    )
  }

  return(ask)
}
