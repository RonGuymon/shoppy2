#' @title Shopify Orders API endpoint
#' @description Gets shopify orders count, all orders, or orders by hour. Returns a max of 250 orders.
#'
#' Documentation: https://help.shopify.com/api/reference/orders/order#index
#' @param shopifyPath Something like: mywebsite.com
#' @param apiKey Unencoded api key
#' @param apiPassword Unencoded apiPassword
#' @param count Defaults to False. If set to True, it will only return the count of orders.
#' @param verbose Whether it will return the results of the api call. Defaults to T.
#' @param createdMin Show orders created at or after date (format: 2014-04-25T16:15:47-04:00).
#' @param createdMax Show orders created at or before date (format: 2014-04-25T16:15:47-04:00).

#' @return If count == T, then a number. Otherwise, a dataframe of orders. This dataframe may contain nested values.
#' @export
#'
getShoppyOrders <- function(shopifyPath, apiKey, apiPassword, count = F, verbose = T, createdMin = NULL, createdMax = NULL){
  shopifyApiKey <- paste0(apiKey,":",apiPassword) %>% jsonlite::base64_enc() %>% gsub("[\r\n]", "", .) %>% paste0("Basic ", .)

  if(count == T){
    apicall <- paste0("https://", shopifyPath, "/admin/orders/count.json")
  }else if(count == F & is.null(createdMin) & is.null(createdMax)){
    apicall <- paste0("https://", shopifyPath, "/admin/orders.json?limit=250")
  }else {
    apicall <- paste0("https://", shopifyPath, "/admin/orders.json?limit=250&created_at_min=", createdMin, "&created_at_max=", createdMax)
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
  if(count == T){
    df <- content(r, "parsed")
  }else{
    df <- content(r, "text") %>% jsonlite::fromJSON() %>% as.data.frame()
  }
  return(df)
}
