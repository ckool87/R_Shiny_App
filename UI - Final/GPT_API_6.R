library(httr)
library(jsonlite)

# Initialize conversation history for AI to know what's going on
history <- list()

openAI_message <- function(message) {
  
  url <- "https://api.openai.com/v1/chat/completions"
  
  # Add user message to history
  history <<- c(history, list(list(role = "user", content = message)))
  
  # API key for authentication
  headers <- c(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer ")
  )
  
  # data to be sent to API
  body <- list(
    model = "gpt-3.5-turbo",
    messages = history
  )
  json_body <- toJSON(body, auto_unbox = TRUE)
  
  # do the thing
  response <- POST(url, add_headers(.headers = headers), body = json_body, encode = "json")
  
  # Check response
  if (status_code(response) == 200) {
    api_response <- content(response, "parsed") # Extract the response text
    chatgpt_response <- api_response$choices[[1]]$message$content
    history <<- c(history, list(list(role = "system", content = chatgpt_response)))
    return(chatgpt_response)
  } else {
    cat("Failed to retrieve response. Status code:", status_code(response), "\n")
    return(content(response, "text"))
  }
}

# testing
# print(openAI_message("what's your favorite color?"))