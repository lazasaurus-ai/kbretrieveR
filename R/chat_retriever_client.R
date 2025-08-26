# R/chat_retriever_client.R

#' Unified client for KB retrieval + chat generation (ellmer)
#'
#' @param kb_id Knowledge Base ID.
#' @param ellmer_chat An ellmer chat client object (e.g., from ellmer::chat_aws_bedrock()).
#' @param profile AWS profile, optional.
#' @param region AWS region, optional.
#' @param retrieval_cfg Retrieval config (`list(topK = 6)` by default).
#' @param system_prompt Optional system prompt to prepend.
#' @param max_context_chars Trim KB context before generation.
#' @param use_memory Logical, keep a rolling buffer of turns.
#' @param memory_k Number of turns to keep.
#' @export
chat_retriever_client <- function(kb_id,
                                  ellmer_chat,
                                  profile = NULL,
                                  region = NULL,
                                  retrieval_cfg = list(topK = 6),
                                  system_prompt = NULL,
                                  max_context_chars = 12000,
                                  use_memory = TRUE,
                                  memory_k = 12) {
  .memory <- tibble::tibble(role = character(), content = character())
  .system <- system_prompt
  
  trim_ctx <- function(x, n) if (nchar(x) > n) substr(x, 1, n) else x
  
  build_prompt <- function(question, refs) {
    ctx <- paste(refs$chunk_text, collapse = "\n---\n")
    ctx <- trim_ctx(ctx, max_context_chars)
    
    mem <- ""
    if (use_memory && nrow(.memory)) {
      keep <- utils::tail(.memory, memory_k)
      mem <- paste(paste0(keep$role, ": ", keep$content), collapse = "\n")
      mem <- paste0("Conversation summary (recent turns):\n", mem, "\n\n")
    }
    
    paste0(
      if (!is.null(.system)) paste0("[System]\n", .system, "\n\n") else "",
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
    
    if (use_memory) {
      .memory <<- dplyr::bind_rows(
        .memory,
        tibble::tibble(role = "user",      content = question),
        tibble::tibble(role = "assistant", content = as.character(ans$text %||% ans))
      )
      if (nrow(.memory) > (memory_k * 2)) {
        .memory <<- utils::tail(.memory, memory_k * 2)
      }
    }
    
    tibble::tibble(
      answer    = as.character(ans$text %||% ans),
      citations = list(refs),
      raw       = list(ans)
    )
  }
  
  save_memory <- function(path) { saveRDS(.memory, path); invisible(path) }
  load_memory <- function(path) {
    m <- readRDS(path)
    stopifnot(is.data.frame(m), all(c("role","content") %in% names(m)))
    .memory <<- tibble::as_tibble(m); invisible(nrow(.memory))
  }
  reset_memory <- function() { .memory <<- tibble::tibble(role=character(), content=character()); invisible(TRUE) }
  set_system   <- function(x) { .system <<- x; invisible(TRUE) }
  get_memory   <- function()  { .memory }
  get_system   <- function()  { .system }
  
  structure(
    list(
      ask           = ask,
      save_memory   = save_memory,
      load_memory   = load_memory,
      reset_memory  = reset_memory,
      set_system    = set_system,
      get_memory    = get_memory,
      get_system    = get_system
    ),
    class = "chat_retriever_client"
  )
}
