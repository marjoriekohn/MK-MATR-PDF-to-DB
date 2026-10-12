# Gemini: reads a PDF and answers in words, as the prompt asks. Requests go
# to generateContent (REST, v1beta) with the PDF and the prompt and nothing
# else: no response schema, as Gemini's schema-following mode stops early on
# sections inside sections. The API key is GEMINI_KEY, loaded from .env by
# global.R. Errors are sentences staff can act on, since they're shown on the
# Extraction page.

gemini_model_url <- "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent"

# The most Gemini writes in one answer, thinking included. Longer questionnaires
# are read a few pages at a time (service_reading.R).
gemini_max_output_tokens <- 65536L

# One request waits this long for its answer, and a busy Gemini (these statuses,
# or no answer at all) is asked again, waiting longer each time from the first
# pause up to the longest, until `give_up_after` seconds have passed.
gemini_timeout_seconds <- 15 * 60
gemini_first_pause_seconds <- 5
gemini_longest_pause_seconds <- 30
gemini_busy_statuses <- c(429, 500, 502, 503, 504)

#' Have Gemini read a PDF.
#'
#' One request for each prompt, all about the same PDF, sent at once. A busy Gemini is
#' asked again (see `gemini_model_perform()`). Errors are sentences staff can act on.
#'
#' @param pdf_path The PDF.
#' @param prompts A character vector, one prompt for each request.
#' @param model The Gemini model to use.
#' @param api_key The Gemini API key; defaults to `GEMINI_KEY`.
#' @return A list with one answer for each prompt: `text`, `model`, `input_tokens`,
#'   `output_tokens` and `thinking_tokens`.
gemini_model_generate <- function(pdf_path, prompts, model = "gemini-3.8-flash",
                                  api_key = Sys.getenv("GEMINI_KEY")) {
  if (!nzchar(api_key)) {
    stop("There's no Gemini API key. Add GEMINI_KEY to the .env file and restart the site.", call. = FALSE)
  }
  sending <- lapply(prompts, function(prompt) gemini_model_request(pdf_path, prompt, model, api_key))
  lapply(gemini_model_perform(sending), function(response) {
    if (inherits(response, "error")) stop("Couldn't reach Gemini: ", conditionMessage(response), call. = FALSE)
    gemini_model_parse(httr2::resp_body_json(response), httr2::resp_status(response))
  })
}

# The PDF first, then the prompt.
gemini_model_request <- function(pdf_path, prompt, model, api_key) {
  body <- list(
    contents = list(list(role = "user", parts = list(
      list(inlineData = list(mimeType = "application/pdf", data = base64enc::base64encode(pdf_path))),
      list(text = prompt)
    ))),
    generationConfig = list(maxOutputTokens = gemini_max_output_tokens)
  )
  httr2::request(sprintf(gemini_model_url, model)) |>
    httr2::req_headers(`x-goog-api-key` = api_key, .redact = "x-goog-api-key") |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(gemini_timeout_seconds) |>
    httr2::req_error(is_error = function(response) FALSE)
}

# Sends the requests at once. Gemini often turns requests this big away as busy
# (429, 503) while small ones get through, and server errors come and go, so
# those are sent again, waiting a little longer each time, for up to
# `give_up_after` seconds. Reading runs in the background, so it can wait.
# (httr2's own req_retry() isn't used: when requests are sent in parallel, this
# version of httr2 retries a busy reply without any limit, and a job that never
# gives up could run for ever.)
gemini_model_perform <- function(requests, give_up_after = gemini_timeout_seconds, pause = gemini_first_pause_seconds) {
  responses <- vector("list", length(requests))
  waiting <- seq_along(requests)
  deadline <- Sys.time() + give_up_after
  repeat {
    sent <- httr2::req_perform_parallel(requests[waiting], on_error = "continue", progress = FALSE)
    responses[waiting] <- sent
    waiting <- waiting[vapply(sent, gemini_busy, logical(1))]
    if (length(waiting) == 0 || Sys.time() + pause > deadline) {
      return(responses)
    }
    Sys.sleep(pause)
    pause <- min(2 * pause, gemini_longest_pause_seconds)
  }
}

gemini_busy <- function(response) {
  inherits(response, "error") || httr2::resp_status(response) %in% gemini_busy_statuses
}

# The text Gemini wrote, with what the request cost.
gemini_model_parse <- function(body, status = 200L) {
  if (status != 200) stop(gemini_problem(body, status), call. = FALSE)
  blocked <- body$promptFeedback$blockReason
  if (!is.null(blocked)) stop("Gemini wouldn't read this PDF (", blocked, ").", call. = FALSE)
  candidate <- body$candidates[[1]]
  finish <- candidate$finishReason %||% "UNKNOWN"
  if (finish == "MAX_TOKENS") {
    stop("The questionnaire is too long for Gemini to write out in one go.", call. = FALSE)
  }
  if (finish != "STOP") stop("Gemini stopped before the end (", finish, ").", call. = FALSE)
  parts <- Filter(function(part) !isTRUE(part$thought), candidate$content$parts)
  text <- paste(vapply(parts, function(part) part$text %||% "", character(1)), collapse = "")
  if (!nzchar(text)) stop("Gemini sent back an empty answer.", call. = FALSE)
  list(
    text = text,
    model = body$modelVersion,
    input_tokens = body$usageMetadata$promptTokenCount %||% 0L,
    output_tokens = body$usageMetadata$candidatesTokenCount %||% 0L,
    thinking_tokens = body$usageMetadata$thoughtsTokenCount %||% 0L
  )
}

gemini_problem <- function(body, status) {
  message <- body$error$message %||% ""
  switch(as.character(status),
    "400" = paste("Gemini couldn't take the request:", message),
    "401" = ,
    "403" = "Gemini turned down the API key. Check GEMINI_KEY in the .env file.",
    "404" = paste("Gemini doesn't know that model:", message),
    "429" = ,
    "503" = "Gemini is busy right now. Try reading again in a few minutes.",
    sprintf("Gemini had a problem (%s): %s", status, message)
  )
}
