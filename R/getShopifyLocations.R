#' @title Shopify Location Endpoint
#' @description Gets shopify products.
#'
#' Documentation: https://help.shopify.com/api/reference/location#index
#' @param shopifyPath Something like: mywebsite.com
#' @param apiKey Unencoded api key
#' @param apiPassword Unencoded apiPassword
#' @param verbose Whether it will return the results of the api call. Defaults to T.
#' @param locationID Id of a specific location in quotes.

#' @return If locationId is missing, then it returns a dataframe about each location. Otherwise, it returns html about a specific location.
#' @export
#'
getShoppyLocations <- function(shopifyPath, apiKey, apiPassword, verbose = T, locationId = NULL){
  shopifyApiKey <- paste0(apiKey,":",apiPassword) %>% jsonlite::base64_enc() %>% gsub("[\r\n]", "", .) %>% paste0("Basic ", .)

  if(is.null(locationId)){
    apicall <- paste0("https://", shopifyPath, "/admin/locations.json")
  }else {
    apicall <- paste0("https://", shopifyPath, "/admin/locations/#", locationId, ".json")
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
  if(is.null(locationId)){
    df <- content(r, "text") %>% jsonlite::fromJSON() %>% as.data.frame()
  }else{
    df <- content(r, "text")
  }
  return(df)
}
