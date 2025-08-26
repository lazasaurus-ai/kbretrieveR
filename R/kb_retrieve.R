# R/kb_retrieve.R

#' Retrieve chunks from a Bedrock Knowledge Base
#'
#' Calls the AWS Bedrock Agent Runtime `retrieve_and_generate()` API
#' and returns a tibble of retrieved chunk text and metadata. This is
#' retrieval-oriented; it ignores the generated answer.
#'
#' @param question A character string with the user query.
#' @param kb_id The Knowledge Base ID to query.
#' @param profile Optional AWS CLI profile to use.
#' @param region Optional AWS region to use.
#' @param retrieval_cfg Optional list of retrieval configuration parameters (e.g., `list(topK = 6)`).
#'
#' @return A tibble with retrieved chunk text and metadata.
#' @export
kb_retrieve <- function(question, kb_id, profile = NULL, region = NULL, retrieval_cfg = list()) {
  stopifnot(is.character(question), length(question) == 1, nchar(question) > 0)
  stopifnot(is.character(kb_id), length(kb_id) == 1, nchar(kb_id) > 0)
  
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
        retrievalConfiguration = retrieval_cfg
        # modelArn intentionally omitted here; we just want the refs
      ))
    )
  )
  
  resp <- ml$retrieve_and_generate(req)
  
  cites <- tryCatch(resp$citations %||% list(), error = function(e) list())
  if (!length(cites)) {
    return(tibble::tibble(
      title = character(), source_uri = character(), score = numeric(),
      chunk_text = character(), doc_id = character(), chunk_id = character()
    ))
  }
  
  rows <- lapply(cites, function(c) {
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
