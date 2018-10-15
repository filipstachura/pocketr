
step1 <- function(consumer_key, callback_url) {
  pocket_url <- "https://getpocket.com/v3/oauth/request"
  body <- list(consumer_key = consumer_key, redirect_uri = callback_url)
  request <- httr::POST(pocket_url, body = body, encode = "form")
  httr::stop_for_status(request)

  code <- httr::content(request)$code
  visit_url <- paste0("https://getpocket.com/auth/authorize?request_token=", code, "&redirect_uri=", callback_url)

  list(
    code = code,
    visit_url = visit_url
  )
}

step2 <- function(consumer_key, code) {
  pocket_auth <- "https://getpocket.com/v3/oauth/authorize"
  body <- list(consumer_key = consumer_key, code = code)
  request <- httr::POST(pocket_auth, body = body, encode = "form")

  err <- httr::headers(request)$`x-error`
  if (!is.null(err)) {
    stop(err)
  }
  httr::stop_for_status(request)

  pocket_url <- "https://getpocket.com/v3/get"
  access_token <- httr::content(request)$access_token
  body <- list(consumer_key = consumer_key, access_token = access_token, state = "all")
  req <- httr::POST(pocket_url, body = body, encode = "form")
  httr::stop_for_status(req)

  httr::content(req)
}

response_to_bookmarks <- function(api_response) {
  common_names <- api_response$list %>% purrr::map(names) %>% purrr::reduce(intersect)
  raw_bookmarks <- api_response$list %>%
    purrr::map(~ .[common_names]) %>%
    do.call(rbind.data.frame, .)
  raw_bookmarks
}