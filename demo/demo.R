chat <- ellmer::chat_aws_bedrock(
  model = "anthropic.claude-3-5-sonnet-2024320620-v1:0"
)
chat$chat("Hello, who are you?")
