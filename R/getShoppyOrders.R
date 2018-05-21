#' @title Shopify Orders API endpoint
#' @description Gets shopify orders by hour.
#'
#' Documentation: https://help.shopify.com/api/reference/orders/order#index
#'
#' Requires the data to already be summarized into daily amounts. Make sure that there are no missing periods of coviate data if covariates are being included in the model.
#' @param shopifyPath Something like: mywebsite.com
#' @param apiKey Unencoded api key
#' @param apiPassword Unencoded apiPassword
#' @param verbose Whether it will return the results of the api call. Defaults to T.

#' @return A dataframe of orders. This dataframe contains nested values.
#' @export
#'
getShoppyOrders <- function(shopifyPath, apiKey, apiPassword, verbose = T){
  shopifyApiKey <- paste0(apiKey,":",apiPassword) %>% base64_enc() %>% gsub("[\r\n]", "", .) %>% paste0("Basic ", .)

  # Ping the api
  if(verbose == T){
    r <- GET(paste0("https://", shopifyPath, "/admin/orders.json?limit=250"),
             add_headers("Authorization" = shopifyApiKey
                         , "Content-Type" = "application/json"
             )
             ,verbose()
    )
  }else{
    r <- GET(paste0("https://", shopifyPath, "/admin/orders.json?limit=250"),
             add_headers("Authorization" = shopifyApiKey
                         , "Content-Type" = "application/json"
             )
    )
  }

  # Parse the data and return a dataframe
  df <- content(re, "text") %>% fromJSON() %>% as.data.frame()
  return(df)
}
