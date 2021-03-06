#' @title Shopify Products Endpoint
#' @description Gets shopify products.
#'
#' Documentation: https://help.shopify.com/api/reference/products#index
#' @param shopifyPath Something like: mywebsite.com
#' @param apiKey Unencoded api key
#' @param apiPassword Unencoded apiPassword
#' @param verbose Whether it will return the results of the api call. Defaults to T.
#' @param createdMin Show orders created at or after date (format: 2014-04-25T16:15:47-04:00).
#' @param createdMax Show orders created at or before date (format: 2014-04-25T16:15:47-04:00).
#' @param page Returns a specific page of results. Defaults to 1.

#' @return Returns a dataframe of products. May contain nested dataframes.
#' @export
#'
getShoppyProducts <- function(shopifyPath, apiKey, apiPassword, verbose = T, createdMin = NULL, createdMax = NULL, page = NULL){
  shopifyApiKey <- paste0(apiKey,":",apiPassword) %>% jsonlite::base64_enc() %>% gsub("[\r\n]", "", .) %>% paste0("Basic ", .)

  if(is.null(createdMin) & is.null(createdMax) & is.null(page)){
    apicall <- paste0("https://", shopifyPath, "/admin/products.json?limit=250")
  }else if(!is.null(page)){
    apicall <- paste0("https://", shopifyPath, "/admin/products.json?limit=250&page=", page)
  }else {
    apicall <- paste0("https://", shopifyPath, "/admin/products.json?limit=250&created_at_min=", createdMin, "&created_at_max=", createdMax)
  }

  if(verbose == T){
    r <- GET(apicall,
             add_headers("Authorization" = shopifyApiKey
                         , "Content-Type" = "application/json"
             )
             ,verbose()
    )
  }else{
    r <- GET(apicall,
             add_headers("Authorization" = shopifyApiKey
                         , "Content-Type" = "application/json"
             )
             # ,verbose()
    )
  }

  # Parse the data and return a dataframe
  df <- content(r, "text") %>% jsonlite::fromJSON() %>% as.data.frame()
  return(df)
}
