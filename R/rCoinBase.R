# *******************************************************************************************
# https://docs.cdp.coinbase.com/coinbase-app/trade/reference
#setwd(getwd())
require('httr');require('jose');require('openssl');require("jsonlite");require("digest")
require('uuid');require('purrr');require("tidyr");require("httr2");require("lubridate")
require("plyr")

# source("build_jwt.R")
# *******************************************************************************************
# saveRDS(list(api_key = "organizations/846d3bac-8e52-48c5-ad3f-57148eb60e57/apiKeys/4f928e1a-8dad-4826-9484-47a7d8346daf",
#      private_key_pem = "-----BEGIN EC PRIVATE KEY-----\nMHcCAQEEIOhKjLMRYbgFRHJT33YK6O4qzQuDQyitSQ8dJmqas2U2oAoGCCqGSM49\nAwEHoUQDQgAETJXuT5cMbf1/s3BxZLb6VaYpcOCo7X5dhw2VHsNhHcd+BJ4D2Pfc\nUlvGdvsioLGemGBbifqIlf1Nz3INKUoa+w==\n-----END EC PRIVATE KEY-----\n",
#      tokens = list(jwt = NULL,
#                    exp_time = NULL
#                    )
#      ), file = "cb_tokens.rds")

#' Assign API KEY/PRIVATE TOKEN INTO R
#'@return reads binary tokens and assigns variable in Global Environment
#'@export
assign_tokens = function(){
  client = readRDS("cb_tokens.rds")
  pw <- new.env()
  assign("jwt", value = client$tokens$jwt, envir = pw)
}
# ******************************************************************************************
#                             HELPER FUNCTIONS
# ******************************************************************************************
#' get a random order ID for orders
#'@return returns random order ID
#'@export
cb_get_order_id = function(){
  # Generate a single random UUID
  id <- UUIDgenerate()
  #print(id)
  
  # Generate multiple UUIDs (e.g., 5)
  #num_uuids <- 5
  #uuids <- UUIDgenerate(n = num_uuids)
  #print(uuids)
  
  # Generate time-based UUIDs
  #time_uuid <- UUIDgenerate(use.time = TRUE)
  #print(time_uuid)
  id
}

# ******************************************************************************************
#                             ACCOUNTS
# ******************************************************************************************
#' List Accounts: Get a list of authenticated Advanced Trade accounts for the current user.
#'@param lmt = The number of accounts to display per page. By default, displays 49 (max 250)
#'@export
cb_getAccounts = function(lmt){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "accounts"
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_url_query(limit = paste0(lmt))
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = lapply(as.list(1:length(content$accounts)), function(i) data.frame(t(as.data.frame(unlist(content[["accounts"]][[i]]))),row.names = NULL))
    df = as.data.frame(do.call(rbind,df))
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# test function
# accts = cb_getAccounts(lmt = 100)

#' Get Accounts: Get a list of information about an account, given an account UUID.
#'@export
cb_getAccount = function(acct_uuid){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  #'@param acct_uuid = The account's UUID.
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("accounts/",acct_uuid)
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_url_query(body="")
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content[[1]]))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# test function
# acct = cb_getAccount(acct_uuid = 'f412dr89-01d0-576d-g457-ea0b52a13716')
# ******************************************************************************************
#                             CONVERTS
# ******************************************************************************************
#' Commit Convert Trade: 
#'@param trade_id = The ID of the trade to commit.
#'@param from_account = The currency of the account to convert from (e.g. USD).
#'@param to_account = The currency of the account to convert to (e.g. USDC).
#'@return Commits a convert trade with a specified trade id, source account, and target account
#'@export
cb_commit_convert_trade = function(trade_id, from_account, to_account){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("convert/trade/",trade_id)
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = list(from_account = paste(from_account), 
              to_account = paste(to_account)
             )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content[[1]]))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Create Convert Quote:
#' Convert is applicable for USDC-USD, EURC-EUR, and PYUSD-USD conversion
#'@param amount = The ID of the trade to commit.
#'@param from_account = The currency of the account to convert from (e.g. USD).
#'@param to_account = The currency of the account to convert to (e.g. USDC).
#'@return Create a convert quote with a specified source account,target account, and amount.
#'@export
cb_create_convert_quote = function(amount, from_account, to_account){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("convert/quote")
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = list(from_account = paste(from_account), 
              to_account = paste(to_account),
              amount = paste(amount)
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content[[1]]))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Get Convert Trade
#'@param trade_id = The ID of the trade to commit.
#'@param from_account = The currency of the account to convert from (e.g. USD).
#'@param to_account = The currency of the account to convert to (e.g. USDC).
#'@return Gets a list of information about a convert trade with a specified trade id, source account, and target account
#'@export
cb_get_convert_trade = function(trade_id, from_account, to_account){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("convert/trade/",trade_id)
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = list(from_account = paste(from_account), 
              to_account = paste(to_account)
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content[[1]]))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

# How to use:
# qte = cb_create_convert_quote(amount = 100, from_account = "USD",to_account = "USDC")
# ord = cb_commit_convert_trade(trade_id = qte$id, from_account = "USD", to_account = "USDC")
# stat= cb_get_convert_trade(trade_id = qte$id, from_account = "USD", to_account = "USDC")
# ******************************************************************************************
#                             FEES
# ******************************************************************************************
#' Get Transaction Summary: Get a summary of transactions with fee tiers, total volume, and fees.
#'@param product_type = Only returns the orders matching this product type.
#'                      By default, returns all product types. 
#'                      ['UNKNOWN_PRODUCT_TYPE', 'SPOT','FUTURE']
#'@param contract_expiry_type = Only returns the orders matching this contract expiry type.
#'                              Only applicable if product_type is set to FUTURE.
#'                              ['UNKNOWN_CONTRACT_EXPIRY_TYPE','EXPIRING', 'PERPETUAL']
#'@param product_venue = Venue for product 
#'                       ['UNKNOWN_VENUE_TYPE','CBE','FCM','INTX']
#'@export
cb_get_fees = function(product_type="UNKNOWN_PRODUCT_TYPE", 
                       contract_expiry_type="UNKNOWN_CONTRACT_EXPIRY_TYPE", 
                       product_venue="UNKNOWN_VENUE_TYPE"){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("transaction_summary")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_url_query(product_type = paste(product_type)) %>%
    req_url_query(contract_expiry_type = paste(contract_expiry_type)) %>%
    req_url_query(product_venue = paste(product_venue))
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# fees = cb_get_fees()
# ******************************************************************************************
#                             FUTURES
# ******************************************************************************************
#' Cancel Pending Futures Sweep
#'@return Cancel the pending sweep of funds from FCM wallet to USD Spot wallet
#'@export
cb_cancel_futures_sweep = function(){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "cfm/sweeps"
  method <- "DELETE"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
  }else{
    # return null if page status code is not 200
    content = NULL
  }
  content
}

#'  Get Current Margin Window
#'@param margin_profile_type = The margin profile type for your account: *MARGIN_PROFILE_TYPE_UNSPECIFIED*
#'@return Get the futures current margin window
#'@export
cb_get_current_margin_window = function(margin_profile_type='MARGIN_PROFILE_TYPE_RETAIL_REGULAR'){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/intraday/current_margin_window")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_url_query(margin_profile_type = paste(margin_profile_type))
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Get Futures Balance Summary
#'@return Get a summary of balances for CFM trading
#'@export
cb_get_futures_balance = function(){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/balance_summary")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Get Futures Position
#'@param product_id = The ticker symbol (e.g. 'BIT-28JUL25-CDE')
#'@return Get positions for a specific CFM product
#'@export
cb_get_futures_position = function(product_id){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/balance_summary/", product_id)
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Get Intraday Margin Setting
#'@return Get the futures intraday margin setting
#'@export
cb_get_intraday_margin = function(){
  # read in tokens
  client = readRDS("cb_tokens.rds")
 
   # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/intraday/margin_setting")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' List Futures Positions 
#'@return Get a list of positions in CFM products
#'@export
cb_list_futures_positions = function(){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/positions")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' List Futures Sweeps 
#'@return Get pending and processing sweeps of funds from FCM wallet to USD Spot wallet
#'@export
cb_list_futures_sweeps = function(){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/sweeps")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Schedule Futures Sweep 
#'@param usd_amount = The amount of USD to be swept. By default, sweeps all available excess funds.
#'@return Schedules a sweep of funds from FCM wallet to USD Spot wallet
#'@export
cb_schedule_futures_sweeps = function(usd_amount){
  # read in tokens
  client = readRDS("cb_tokens.rds")
 
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/sweeps/schedule")
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = list(usd_amount = paste(usd_amount))
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}

#' Set Intraday Margin Setting
#'@param setting = The amount of USD to be swept. By default, sweeps all available excess funds.
#'@return Set the futures intraday margin setting
#'@export
cb_set_intraday_margin = function(setting){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # The margin setting for the account. 
  # Describes whether the account is opted in to receive increased leverage during weekdays (8am-4pm ET), excluding market holidays.
  # Examples: ['INTRADAY_MARGIN_SETTING_UNSPECIFIED', 'INTRADAY_MARGIN_SETTING_STANDARD', 'INTRADAY_MARGIN_SETTING_INTRADAY']
  # Base URL and endpoint
  
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("cfm/intraday/margin_setting")
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = list(setting = paste(setting))
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' close position
#'@param client_order_id = defaults to random id via *cb_get_order_id()*
#'@param product_id = futures contract to close
#'@param size = number of contracts to close, defaults to closing all available
#'@return returns status of order
#'@export
cb_close_order = function(client_order_id=cb_get_order_id(), product_id, size=NULL){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders/close_position"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  if(!is.null(size)){
    # construct body
    body = list(client_order_id = paste(client_order_id),
                product_id = paste(product_id),
                size = paste(size)
    )
  }else{
    body = list(client_order_id = paste(client_order_id),
                product_id = paste(product_id)
    )
  }
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  
  # pg <- POST(url = "https://api.coinbase.com/api/v3/brokerage/orders/close_position", 
  #            httr::add_headers(c("Authorization" = paste("Bearer", pw$jwt),
  #                                "Content-Type" = "application/json",
  #                                "Connection" = "close",
  #                                "User-Agent" = "coinbase-advanced-r/4.5.0" )), 
  #            body = toJSON(body, auto_unbox = T)) 
  # 
  # content(pg)
  # 
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# ******************************************************************************************
# ORDERS
#'builds body of the order depending on the order type
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param             leverage = (string) The amount of leverage for the order (default is 1.0). Example: 2.0
#'@param          margin_type = (string) Margin Type for this order (default is CROSS). Possible values: [CROSS, ISOLATED]
#'@param           preview_id = (string) Preview ID for this order, to associate this order with a preview request. Example: b40bbff9-17ce-4726-8b64-9de7ae57ad26
#'@param           quote_size = (string) The amount of the second Asset in the Trading Pair. Example: 10.00
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param           start_time = (RFC3339 Timestamp) Time at which the order should begin executing. Example: 2021-05-31T07:59:59Z
#'@param             end_time = (RFC3339 Timestamp) The time at which the order will be cancelled if it is not Filled. Example: 2021-05-31T09:59:59Z
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param       number_buckets = (string) The number of smaller buckets/suborders over which the entire order will be broken into. Each suborder will be executed over a duration calculated based on the end_time. Example: 5
#'@param          bucket_size = (string)  The size of each suborder. bucket_size multiplied by number_buckets should match the size of the entire twap order)
#'@param      bucket_duration = (string) The duration over which each sub order was executed. Example: 300s
#'@param         rfq_disabled = (boolean) If true, this order will be attempted to be routed to the exchange CLOB. 
#'@param            post_only = (boolean) Enable or disable Post-only Mode. When enabled, only Maker Orders will be posted to the Order Book. Orders that will be posted as a Taker Order will be rejected.
#'@param           stop_price = (string) The specified price that will trigger the placement of the Order. Example: 20000.00
#'@param       stop_direction = (string) The direction of the stop limit Order. Possible values: [STOP_DIRECTION_STOP_UP, STOP_DIRECTION_STOP_DOWN]
#'@param   stop_trigger_price = (string) The price level (in quote currency) where the position will be exited. When triggered, a stop limit order is automatically placed with a limit price 5% higher for BUYS and 5% lower for SELLS. Example: 20000.00
#'@return returns order configuration depending on the order used
#'@export
cb_order_builder = function(order_type, client_order_id, product_id, side, leverage = 1.0, margin_type='CROSS',preview_id=NULL,
                            base_size=NULL, quote_size=NULL, start_time=NULL, end_time=NULL, limit_price=NULL, 
                            number_buckets=NULL, bucket_duration=NULL, bucket_size = NULL, rfq_disabled=TRUE, post_only=FALSE,
                            stop_price=NULL, stop_direction=NULL, stop_trigger_price=NULL){
  
  # ************************************************************************************************************************************************************************
  # build object
  #'@param  market_market_ioc = (object)  Buy or sell a specified quantity of an Asset at the current best available market price. 
  if(order_type == 'market_market_ioc'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  market_market_ioc = list(
                    base_size = paste(base_size)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param      sor_limit_ioc = (object) Buy or sell a specified quantity of an Asset at a specified price. The Order will only post to the Order Book if it will immediately Fill; any remaining quantity is canceled
  if(order_type == 'sor_limit_ioc'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  sor_limit_ioc = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param    limit_limit_gtc = (object) Buy or sell a specified quantity of an Asset at a specified price. If posted, the Order will remain on the Order Book until canceled.
  if(order_type == 'limit_limit_gtc'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  limit_limit_gtc = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param    limit_limit_gtd = (object) Buy or sell a specified quantity of an Asset at a specified price. If posted, the Order will remain on the Order Book until a certain time is reached or the Order is canceled
  if(order_type == 'limit_limit_gtd'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  limit_limit_gtd = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price),
                    end_time = paste(end_time)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param    limit_limit_fok = (object) Buy or sell a specified quantity of an Asset at a specified price. The Order will only post to the Order Book if it is to immediately and completely Fill
  if(order_type == 'limit_limit_fok'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  limit_limit_fok = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param     twap_limit_gtd = (object) A time-weighted average price (TWAP) order type that calculates the average price of a product to programmatically execute an order over a specified duration.
  if(order_type == 'twap_limit_gtd'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  twap_limit_gtd = list(
                    base_size = paste(base_size),
                    start_time = paste(start_time),
                    end_time = paste(end_time),
                    limit_price = paste(limit_price),
                    number_buckets = paste(number_buckets),
                    bucket_size = paste(bucket_size),
                    bucket_duration = paste(bucket_duration)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param  stop_limit_stop_limit_gtc = (object) Posts an Order to buy or sell a specified quantity of an Asset, but only if and when the last trade price on the Order Book equals or surpasses the Stop Price. If posted, the Order will remain on the Order Book until canceled
  if(order_type == 'stop_limit_stop_limit_gtc'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  stop_limit_stop_limit_gtc = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price),
                    stop_price = paste(stop_price),
                    stop_direction = paste(stop_direction)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param  stop_limit_stop_limit_gtd = (object) Posts an Order to buy or sell a specified quantity of an Asset, but only if and when the last trade price on the Order Book equals or surpasses the Stop Price.
  if(order_type == 'stop_limit_stop_limit_gtd'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  stop_limit_stop_limit_gtd = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price),
                    stop_price = paste(stop_price),
                    end_time = paste(end_time),
                    stop_direction = paste(stop_direction)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param  trigger_bracket_gtc = (object) A Limit Order to buy or sell a specified quantity of an Asset at a specified price, with stop limit order parameters embedded in the order. If posted, the Order will remain on the Order Book until canceled.
  if(order_type == 'trigger_bracket_gtc'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  trigger_bracket_gtc = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price),
                    stop_trigger_price = paste(stop_trigger_price)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  #'@param  trigger_bracket_gtd = (object) A Limit Order to buy or sell a specified quantity of an Asset at a specified price, with stop limit order parameters embedded in the order. If posted, the Order will remain on the Order Book until a certain time is reached or the Order is canceled.
  if(order_type == 'trigger_bracket_gtd'){
    # construct body
    body = list(client_order_id = paste(client_order_id), 
                product_id = paste(product_id), 
                side = paste(side),
                order_configuration = list(
                  trigger_bracket_gtd = list(
                    base_size = paste(base_size),
                    limit_price = paste(limit_price),
                    stop_trigger_price = paste(stop_trigger_price),
                    end_time = paste(end_time)
                  )
                )
    )
  }
  # ************************************************************************************************************************************************************************
  body
}
# ******************************************************************************************
# ******************************************************************************************
#' Market Orders
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@return Returns order ID if successful
#'@export
cb_mkt_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'market_market_ioc', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size
                         )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Limit IOC
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@return Returns order ID if successful
#'@export
cb_sor_lmt_ioc_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  # construct body
  body = cb_order_builder(order_type = 'sor_limit_ioc', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          #quote_size = paste(50.00),
                          limit_price = limit_price,
                          rfq_disabled = rfq_disabled
  )
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Limit GTC
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@return Returns order ID if successful
#'@export
cb_lmt_gtc_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  # construct body
  body = cb_order_builder(order_type = 'limit_limit_gtc', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price
                          )
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Limit GTD
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param             order_exp = (TimeStamp) Enter the time you wish to cancel if not filled: Ex. Sys.time()+minutes(5)
#'@return Returns order ID if successful
#'@export
cb_lmt_gtd_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price, order_exp){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # time must be in this format:
  end_time <- format(order_exp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'limit_limit_gtd', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price,
                          end_time = end_time)
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Limit FOK
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@return Returns order ID if successful
#'@export
cb_lmt_fok_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'limit_limit_fok', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price
                          )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Limit TWAP
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          order_start = (TimeStamp) Enter the time you wish to cancel if not filled: Ex. Sys.time()+minutes(5)
#'@param            order_exp = (TimeStamp) Enter the time you wish to cancel if not filled: Ex. Sys.time()+minutes(10)
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param       number_buckets = (string) The number of smaller buckets/suborders over which the entire order will be broken into. Each suborder will be executed over a duration calculated based on the end_time. Example: 5
#'@param      bucket_duration = (string) The duration over which each sub order was executed. Example: 300s
#'@return Returns order ID if successful
#'@export
cb_lmt_twap_gtd_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, order_start, order_exp, limit_price, number_buckets, bucket_duration){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Format Times
  start_time <- format(order_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  end_time <- format(order_exp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'twap_limit_gtd', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          start_time = start_time,
                          end_time = end_time,
                          limit_price = limit_price,
                          number_buckets = number_buckets,
                          bucket_size = base_size/number_buckets, #bucket_size,
                          bucket_duration = paste0(bucket_duration,"s")
                          )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Stop Limit - GTC
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param           stop_price = (string) The specified price that will trigger the placement of the Order. Example: 20000.00
#'@param       stop_direction = (string) The direction of the stop limit Order. Possible values: [STOP_DIRECTION_STOP_UP, STOP_DIRECTION_STOP_DOWN]#'@return Returns order ID if successful
#'@return Returns order ID if successful
#'@export
cb_stp_lmt_gtc_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price, stop_price, stop_direction){
  # read in tokens
  client = readRDS("cb_tokens.rds")

  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'stop_limit_stop_limit_gtc', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price,
                          stop_price = stop_price,
                          stop_direction = stop_direction
                          )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Stop Limit - GTD
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param           stop_price = (string) The specified price that will trigger the placement of the Order. Example: 20000.00
#'@param            order_exp = (TimeStamp) Enter the time you wish to cancel if not filled: Ex. Sys.time()+minutes(10)
#'@param       stop_direction = (string) The direction of the stop limit Order. Possible values: [STOP_DIRECTION_STOP_UP, STOP_DIRECTION_STOP_DOWN]#'@return Returns order ID if successful
#'@return Returns order ID if successful
#'@export
cb_stp_lmt_gtd_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price, stop_price, ord_exp, stop_direction){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # time that order wil expire
  end_time <- format(ord_exp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'stop_limit_stop_limit_gtd', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price,
                          stop_price = stop_price,
                          end_time = end_time,
                          stop_direction = stop_direction
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Trigger Bracket GTC
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param   stop_trigger_price = (string) The price level (in quote currency) where the position will be exited. When triggered, a stop limit order is automatically placed with a limit price 5% higher for BUYS and 5% lower for SELLS. Example: 20000.00
#'@return Returns order ID if successful
#'@export
cb_trig_gtc_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price, stop_trigger_price){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'trigger_bracket_gtc', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price,
                          stop_trigger_price = stop_trigger_price
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Trigger Bracket GTD
#'@param      client_order_id = (string) A unique ID provided for the order (used for identification purposes) Example: 0000-00000-000000
#'@param           product_id = (string) The trading pair (e.g. 'BTC-USD'). Example: BTC-USD
#'@param                 side = (string) The side of the market that the order is on (e.g. 'BUY', 'SELL'). Possible values: [BUY, SELL]
#'@param            base_size = (string) The amount of the first Asset in the Trading Pair. Example: 0.001
#'@param          limit_price = (string) The specified price, or better, that the Order should be executed at. A Buy Order will execute at or lower than the limit price. A Sell Order will execute at or higher than the limit price. Example: 10000.00
#'@param   stop_trigger_price = (string) The price level (in quote currency) where the position will be exited. When triggered, a stop limit order is automatically placed with a limit price 5% higher for BUYS and 5% lower for SELLS. Example: 20000.00
#'@param            order_exp = (TimeStamp) Enter the time you wish to cancel if not filled: Ex. Sys.time()+minutes(10)
#'@return Returns order ID if successful
#'@export
cb_trig_gtd_order = function(client_order_id=cb_get_order_id(), product_id, side, base_size, limit_price, stop_trigger_price, order_exp){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  
  end_time <- format(order_exp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  # construct body
  body = cb_order_builder(order_type = 'trigger_bracket_gtd', 
                          client_order_id = client_order_id, 
                          product_id = product_id, 
                          side=side, 
                          base_size = base_size, 
                          limit_price = limit_price,
                          stop_trigger_price = stop_trigger_price,
                          end_time = end_time
  )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
#' Cancel Order
#'@param            order_ids = (string) enter the order id that you wish to cancel
#'@return Returns status of order placed
#'@export
cb_cancel_order = function(order_ids){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "orders/batch_cancel"
  method <- "POST"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  # construct
  body = list(order_ids = list(order_ids)) 
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>%
    req_body_json(body, auto_unbox = T)
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = data.frame(t(as.data.frame(unlist(content))),row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# ******************************************************************************************
# ******************************************************************************************
#' Get Best Bid/Ask : Get the best bid/ask for all products. 
#' A subset of all products can be returned instead by using the product_ids input.
#'@param ids = vector of product id(s) Example: "BTC-USD" OR c("BTC-USD","ETH-USD")
#'@return Returns status of order placed
#'@export
cb_quote= function(ids){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "best_bid_ask"
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) %>% 
    req_url_query(!!!setNames(as.list(ids), rep("product_ids", length(ids))))
  
  # Perform request
  resp <- req %>% req_perform(verbosity = 0)
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    #df = rbindlist(lapply(as.list(1:length(content[[1]])), function(i) as.data.frame(do.call(cbind,(content[[1]][[i]])))), use.names = T, fill = T)
    # df = do.call(rbind, lapply(content[["pricebooks"]], function(x) unlist(as.data.frame(do.call(cbind, x )))))
    # df = df %>% unnest(cols = everything())
    #df = purrr::map_df(content[[1]], ~ as.data.frame(do.call(cbind, .x))) %>% as.data.frame
    df <- content$pricebooks %>%
      map_dfr(function(x) {
        tibble(
          product_id = x$product_id,
          bid_price = x$bids[[1]]$price %||% NA,  # Handle empty/missing bids
          bid_size = x$bids[[1]]$size %||% NA,
          ask_price = x$asks[[1]]$price %||% NA,  # Handle empty/missing asks
          ask_size = x$asks[[1]]$size %||% NA,
          time = x$time
        )
      }) %>%
      as.data.frame()  # Ensure output is a base R data frame
    # add mid-price as mark
    df$mark = (as.numeric(df$bid_price) + as.numeric(df$ask_price))/2
    df = df[,c("product_id","bid_price","bid_size","mark","ask_price","ask_size","time")]
    df$time = with_tz(as.POSIXct(df$time, "%Y-%m-%dT%H:%M:%OS", tz="UTC"), tzone = Sys.timezone())
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# ******************************************************************************************
# ******************************************************************************************
#' Get Order
#'@param order_id = The ID of the order
#'@return Returns single order by ID
#'@export
cb_getOrder = function(id){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("orders/historical/",id)
  method <- "GET"
  
  # Generate JWT
    build_jwt(
      key_var = client$api_key,
      secret_var = client$private_key_pem,
      method = method,
      endpoint = endpoint
    )
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method)
  
  # Perform request
  resp <- req %>% req_perform(verbosity = 0)
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    
    # return details as a data frame
    df = as.data.frame(rbind(unlist(content)))
    # format column names
    df$order.order_configuration.market_market_ioc.base_size  = as.numeric(df$order.order_configuration.market_market_ioc.base_size)  
      df$order.completion_percentage  = as.numeric(df$order.completion_percentage)  
                df$order.filled_size  = as.numeric(df$order.filled_size)  
       df$order.average_filled_price  = as.numeric(df$order.average_filled_price)  
            df$order.number_of_fills  = as.numeric(df$order.number_of_fills)  
               df$order.filled_value  = as.numeric(df$order.filled_value)  
                  df$order.total_fees = as.numeric(df$order.total_fees)
    df$order.total_value_after_fees   = as.numeric(df$order.total_value_after_fees)
    df$order.outstanding_hold_amount  = as.numeric(df$order.outstanding_hold_amount)
     
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# ******************************************************************************************
# ******************************************************************************************
#' List Products : Get a list of the available currency pairs for trading.
#'@return Returns list of of available crypto assets
#'@export
cb_getCryptoList = function(){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- "products"
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  
  # Create request
  req <- request(base_url) %>%
    req_url_path_append(endpoint) %>%
    req_headers(
      "Authorization" = paste("Bearer", pw$jwt),
      "Content-Type" = "application/json",
      "Connection" = "close",
      "User-Agent" = "coinbase-advanced-r/4.5.0"  
    ) %>%
    req_method(method) 
  
  # Perform request
  resp <- req %>% req_perform()
  # close all connections
  closeAllConnections()
  # check page status code
  if(httr2::resp_status(resp) == 200){
    # Check response
    content <- resp_body_json(resp)
    # return details as a data frame
    df = rbindlist(lapply(content[[1]], function(x) as.data.frame(do.call(cbind, x))), use.names = TRUE, fill = TRUE) %>% as.data.frame
    df = df %>% unnest(cols = everything())
  }else{
    # return null if page status code is not 200
    df = NULL
  }
  df
}
# ******************************************************************************************
# ******************************************************************************************
#' Get Product Candles : Get rates for a single product by product ID, grouped in buckets.
#' The number of candle buckets to be returned. By default, returns 350 (max 350).
#'@param  product_id = The trading pair (e.g. 'BTC-USD').
#'@param  start_time = The UNIX timestamp indicating the start of the time interval.
#'@param    end_time = The UNIX timestamp indicating the end of the time interval.
#'@param    bar_size = The timeframe each candle represents. Examples: [ONE_MINUTE, FIVE_MINUTE, FIFTEEN_MINUTE, THIRTY_MINUTE, ONE_HOUR, TWO_HOUR, SIX_HOUR, ONE_DAY]
#'@return  returns OHLCV for cryptocurrencies
#'@export
cb_bars= function(product_id, start_time, end_time, bar_size){
  # read in tokens
  client = readRDS("cb_tokens.rds")
  
  start_time = as.integer(with_tz(as.POSIXct(start_time, tz = Sys.timezone()), tzone = "UTC"))
  end_time = as.integer(with_tz(as.POSIXct(end_time, tz = Sys.timezone()), tzone = "UTC"))
  
  # Base URL and endpoint
  base_url <- "https://api.coinbase.com/api/v3/brokerage"
  endpoint <- paste0("products/",product_id, "/candles")
  method <- "GET"
  
  # Generate JWT
  build_jwt(
    key_var = client$api_key,
    secret_var = client$private_key_pem,
    method = method,
    endpoint = endpoint
  )
  
  
  # Perform request
  resp = VERB(verb = method, 
              url = paste0(base_url,"/",endpoint,"?start=", start_time,"&end=", end_time, "&granularity=", bar_size), 
              httr::add_headers("Authorization" = paste("Bearer", pw$jwt),
                                "Content-Type" = "application/json",
                                "Connection" = "close",
                                "User-Agent" = "coinbase-advanced-r/4.5.0"), 
              encode = "json")
  # close all connections
  closeAllConnections()
  
  # check page status code
  if(httr::status_code(resp) == 200){
    # Check response
    content <- httr::content(resp)
    # return details as a data frame
    df = do.call(rbind, lapply(content[["candles"]], function(x) as.data.frame(do.call(cbind, x ))))
    df = df %>% unnest(cols = everything())
    df = as.data.frame(apply(df, MARGIN = 2, FUN = as.numeric))
    df = df[,c("start","open","high","low","close","volume")]
    df$sym = product_id
    df = df[order(df$start, decreasing = F),]
    df$time = with_tz(as.POSIXct(df$start, tz="UTC"), tzone = Sys.timezone())
    df = data.frame(df, row.names = NULL)
    
  }else{
    # return null if page status code is not 200
    cat("\n", content(resp))
    df = NULL
  }
  
  df
}
#' cb_bars(product_id = "ETH-USD", start_time = Sys.time()-hours(1), end_time = Sys.time(), bar_size = 'FIVE_MINUTE')

#' Use this function if 
#'@param  product_id = The trading pair (e.g. 'BTC-USD').
#'@param    start = start date to get data. Ex. Sys.Date()-60 
#'@param    end = End date to get data. Ex. Sys.Date()
#'@param    bar_size = The timeframe each candle represents. Examples: [ONE_MINUTE, FIVE_MINUTE, FIFTEEN_MINUTE, THIRTY_MINUTE, ONE_HOUR, TWO_HOUR, SIX_HOUR, ONE_DAY]
#'@return OHLCV for longer periods
#'@export
cb_candles = function(product_id, start, end, bar_size){
  # set bar size values
  if(bar_size == 'ONE_MINUTE'){BY = '1 min'}
  if(bar_size == 'FIVE_MINUTE'){BY = '5 mins'}
  if(bar_size == 'FIFTEEN_MINUTE'){BY = '15 mins'}
  if(bar_size == 'THIRTY_MINUTE'){BY = '30 mins'}
  if(bar_size == 'ONE_HOUR'){BY = '1 hour'}
  if(bar_size == 'TWO_HOUR'){BY = '2 hours'}
  if(bar_size == 'SIX_HOUR'){BY = '6 hours'}
  if(bar_size == 'ONE_DAY'){BY = '1 day'}
  
  # bar sequence
  DATES = seq.POSIXt(as.POSIXct(paste0(start, "00:00:00"), tz = Sys.timezone()), 
                     to = as.POSIXct(as.POSIXct(paste0(end, "23:59:59"), tz = Sys.timezone())),
                     by = BY)
  
  if(length(DATES) > 350){
    # just get 1st and last timestamp
    STARTS = seq(1, length(DATES), 350)
    ENDS = c(seq(350, length(DATES),350),length(DATES))
  }else{
    STARTS = c(1)          #DATES[1]
    ENDS = c(length(DATES)) #DATES[length(DATES)]
  }
  
  # for each get data
  dta = lapply(as.list(1:length(STARTS)), function(i){
    # sub STARTS/END
    this_start = DATES[STARTS[i]]
    this_end = DATES[ENDS[i]]
    # get crypto bars
    bars = try(cb_bars(product_id=product_id, start_time = as.integer(this_start), end_time = as.integer(this_end), bar_size = bar_size), silent=TRUE)
    # return
    bars
  })
  # row bind results
  dta = dta[lapply(dta, length)>0]
  dta = do.call(rbind, dta)
  # return
  dta
}