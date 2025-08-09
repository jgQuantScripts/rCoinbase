## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

saveRDS(list(api_key = "organizations/846d3bac-8e52-48c5-ad3f-57148eb60e57/apiKeys/4f928e1a-8dad-4826-9484-47a7d8346daf",
     private_key_pem = "-----BEGIN EC PRIVATE KEY-----\nMHcCAQEEIOhKjLMRYbgFRHJT33YK6O4qzQuDQyitSQ8dJmqas2U2oAoGCCqGSM49\nAwEHoUQDQgAETJXuT5cMbf1/s3BxZLb6VaYpcOCo7X5dhw2VHsNhHcd+BJ4D2Pfc\nUlvGdvsioLGemGBbifqIlf1Nz3INKUoa+w==\n-----END EC PRIVATE KEY-----\n",
     tokens = list(jwt = NULL,
                   exp_time = NULL
                   )
     ), file = "cb_tokens.rds")

readRDS("cb_tokens.rds")

