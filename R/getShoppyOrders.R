#' @title Shopify Orders API endpoint
#' @description Gets shopify orders by hour.
#'
#' Documentation: https://help.shopify.com/api/reference/orders/order#index
#'
#' Requires the data to already be summarized into daily amounts. Make sure that there are no missing periods of coviate data if covariates are being included in the model.
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

  # Ping the api
  if(count == T){
    if(verbose == T){
      r <- GET(paste0("https://", shopifyPath, "/admin/orders/count.json"),
               add_headers("Authorization" = shopifyApiKey
                           , "Content-Type" = "application/json"
               )
               ,verbose()
      )
    }else{
      r <- GET(paste0("https://", shopifyPath, "/admin/orders/count.json"),
               add_headers("Authorization" = shopifyApiKey
                           , "Content-Type" = "application/json"
               )
      )

    }
    df <- content(r, "parse")
  }else if(count == F){
    # All orders
    if(is.null(createdMin) & is.null(createdMax)){

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
      df <- content(r, "text") %>% jsonlite::fromJSON() %>% as.data.frame()
    }else {
      # Orders by hour

      if(verbose == T){
        r <- GET(paste0("https://", shopifyPath, "/admin/orders.json?limit=250&created_at_min=", createdMin, "&created_at_max=", createdMax),
                 add_headers("Authorization" = shopifyApiKey
                             , "Content-Type" = "application/json"
                 )
                 ,verbose()
        )
      }else{
        r <- GET(paste0("https://", shopifyPath, "/admin/orders.json?limit=250&created_at_min=", createdMin, "&created_at_max=", createdMax),
                 add_headers("Authorization" = shopifyApiKey
                             , "Content-Type" = "application/json"
                 )
        )
      }

      # Parse the data and return a dataframe
      df <- content(r, "text") %>% jsonlite::fromJSON() %>% as.data.frame()
    }
  }
  return(df)
}
