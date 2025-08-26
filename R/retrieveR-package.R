#' retrieveR: AWS Bedrock Knowledge Base helpers for R
#'
#' @description
#' `retrieveR` provides utilities to call AWS Bedrock Agent Runtime for
#' Knowledge Base retrieval and (optionally) generate answers via a chat client
#' such as [ellmer::chat_aws_bedrock()]. It is designed for simple
#' Retrieval-Augmented Generation (RAG) flows.
#'
#' Main functions include:
#' - [kb_retrieve()] – retrieve relevant chunks from a Knowledge Base
#' - [kb_ask()] – retrieve + generate (Bedrock RnG API)
#' - [chat_retriever_client()] – unified client combining KB retrieval with
#'   an `ellmer` chat object and optional conversational memory
#'
#' @keywords internal
"_PACKAGE"
