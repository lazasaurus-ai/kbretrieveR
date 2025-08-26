# R/kb_ask.R

#' Retrieve and generate from a Bedrock Knowledge Base
#'
#' Calls Bedrock Agent Runtime `retrieve_and_generate()` to both retrieve context
#' and generate an answer. Returns a tidy tibble (answer, citations, usage, raw).
#'
#' @inheritParams kb_retrieve
#' @param model_id Optional model ARN to use for generation.
#' @param generation_cfg Optional list (e.g., `list(maxTokens = 4000, temperature = 0)`).
#' @param guardrails_cfg Optional guardrail configuration list.
#' @param return One of `"tidy"` or `"raw"`. Default `"tidy"`.
#'
#' @return A tibble (tidy) or the raw paws response.
#' @export
kb_ask <- function(question,
                   kb_id,
                   model_id = NULL,
                   profile = NULL,
                   region = NULL,
                   retrieval_cfg = list(),
                   generation_cfg = list(),
                   guardrails_cfg = NULL,
                   return = c("tidy", "raw")) {
  return <- match.arg(return)
  
  ml <- paws.machine.learning::bedrockagentruntime(config = compact(list(
    credentials = if (!is.null(profile)) list(profile = profile),
    region      = region
  )))
  
  req <- list(
    input = list(text = question),
    retrieveAndGenerateConfiguration = list(
      type = "KNOWLEDGE_BASE",
      knowledgeBaseConfiguration = compact(list(
        knowledgeBaseId        = kb_id,
        modelArn               = model_id,
        retrievalConfiguration = retrieval_cfg
      ))
    )
  )
  
  if (length(generation_cfg)) req$generationConfiguration <- generation_cfg
  if (!is.null(guardrails_cfg)) req$guardrailConfiguration <- guardrails_cfg
  
  resp <- ml$retrieve_and_generate(req)
  if (return == "raw") return(resp)
  
  answer <- tryCatch(resp$output$text %||% NA_character_, error = function(e) NA_character_)
  
  cites_list <- tryCatch(resp$citations %||% list(), error = function(e) list())
  cites <- if (!length(cites_list)) {
    tibble::tibble()
  } else {
    rows <- lapply(cites_list, function(c) {
      r <- c$retrievedReferences[[1]]
      tibble::tibble(
        title      = r$metadata$title %||% NA_character_,
        source_uri = r$location$s3Location$uri %||% r$location$webLocation$url %||% NA_character_,
        score      = r$score %||% NA_real_,
        chunk_text = r$content$text %||% NA_character_,
        doc_id     = r$metadata$documentId %||% NA_character_,
        chunk_id   = r$metadata$chunkId %||% NA_character_
      )
    })
    dplyr::bind_rows(rows)
  }
  
  usage <- tryCatch(resp$usage %||% list(), error = function(e) list())
  
  tibble::tibble(
    answer    = answer,
    citations = list(cites),
    usage     = list(usage),
    raw       = list(resp)
  )
}
