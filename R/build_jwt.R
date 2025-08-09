# ******************************************************************************************
#                             GETS BEARER TOKEN
# ******************************************************************************************
#' Gets Bearer Token
#'@param key_var =  your personal API key
#'@param secret_var = your personal secret token 
#'@param method = GET, POST, etc.
#'@param endpoint = endpoint to use
#'@return returns JWT token to make API requests
#'@export
build_jwt <- function(key_var, secret_var, method, endpoint) {
  
  client = readRDS("cb_tokens.rds")
  # build uri
  uri = paste0(method, " api.coinbase.com/api/v3/brokerage/", endpoint)
  
  # Validate inputs
  if(is.null(key_var) || key_var == "" || !is.character(key_var)) { stop("API key is missing, empty, or not a character string") } 
  if(is.null(secret_var) || secret_var == "" || !is.character(secret_var)) { stop("Private key is missing, empty, or not a character string") }
  
  # Normalize newlines
  secret_var <- gsub("\r\n|\r", "\n", secret_var)
  
  # Validate PEM format
  if(!grepl("^-----BEGIN EC PRIVATE KEY-----\n", secret_var) || !grepl("\n-----END EC PRIVATE KEY-----\n?$", secret_var)){
    stop("Invalid PEM format: Must start with '-----BEGIN EC PRIVATE KEY-----\n' and end with '\n-----END EC PRIVATE KEY-----'") 
  }
  
  # Parse private key
  private_key <- tryCatch({ 
    key <- openssl::read_key(textConnection(secret_var)) 
    if(is.null(key)) { stop("Parsed key is NULL") } 
    key 
  }, error = function(e) { stop("Failed to parse private key: ",
                                e$message, "\nEnsure secret_var is a valid ECDSA private key in PEM format") })
  
  # Verify P-256 curve
  tryCatch({ 
    pubkey <- openssl::write_pem(private_key$pubkey)
    curve_verified <- FALSE 
    if(!is.null(private_key$data$curve)) { 
      if(private_key$data$curve %in% c("P-256", "prime256v1", "secp256r1")) { 
        curve_verified <- TRUE } } 
    if(!curve_verified){ 
      warning("Key curve could not be verified as P-256. Proceeding with caution.") }},
    error = function(e){stop("Failed to verify key: ", e$message) })
  
  # Current timestamp
  now <- as.numeric(Sys.time())
  
  # Generate nonce (64-char hex, 32 bytes)
  nonce <- paste0(sample(c(0:9, letters[1:6]), 64, replace = TRUE), collapse = "")
  
  # JWT header
  #header <- list( alg = "ES256", kid = key_var, typ = "JWT", nonce = nonce )
  header <- list(kid = key_var, nonce = nonce )
  
  jwt_claim2 = function (iss = NULL, sub = NULL, aud = NULL, exp = NULL, nbf = NULL) 
  {
    values <- list(iss = (iss), sub = (sub), 
                   aud = (aud), exp = (exp), 
                   nbf = (nbf))
    structure(Filter(function(x) {
      is.list(x) || length(x)
    }, values), class = c("jwt_claim", "list"))
  }
  
  # JWT payload
  #payload <- jose::jwt_claim( sub = key_var, iss = "cdp", nbf = floor(now), exp = floor(now + 120))
  payload = jwt_claim2( sub = key_var, iss = "cdp", nbf = floor(now), exp = floor(now + 120))
  
  # Add uri if provided
  if (!is.null(uri)) { payload$uri <- uri }
  
  # Generate JWT
  jwt <- tryCatch({
    jose::jwt_encode_sig( claim = payload, key = private_key, header = header ) 
  }, 
  error = function(e){ 
    stop("Failed to encode JWT: ", e$message) 
  })
  
  # Calculate expiration time
  expiry_time <- as.POSIXct(now + 90, origin = "1970-01-01")
  
  client$tokens$jwt = jwt
  client$tokens$exp_time = expiry_time
  saveRDS(client, 'cb_tokens.rds')
  assign("jwt", value = jwt, envir = pw)
  #jwt
}