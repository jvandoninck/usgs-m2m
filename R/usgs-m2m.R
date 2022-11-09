
##  R implementation for USGS/EROS Inventory Service (Machine-to-Machine API )
##    
##  Requires USGS M2M access, see https://m2m.cr.usgs.gov/login/

##


##  Get service URL
m2mServiceUrl <- function(tier="stable"){
  if(tier=="stable") return("https://m2m.cr.usgs.gov/api/api/json/stable/")
  if(tier=="experimental") return("https://m2m.cr.usgs.gov/api/api/json/experimental/")
  if(tier=="development") return("https://m2m.cr.usgs.gov/api/api/json/development/")
  stop('"tier" must be one of ["stable", "experimental", "development"]')
}


##  Send http request
sendRequest <- function(url, data, apiKey=NULL){
  require(httr)
  
  if(is.null(apiKey)){
    response <- POST(url, body=data, encode="json")
  } else {
    response <- POST(url, body=data, encode="json", add_headers('X-Auth-Token' = apiKey))
  }
  
  #check response for errors
  tryStatus <- try(httpStatusCode <- response$status_code, TRUE)
  if(inherits(tryStatus, "try-error")) stop(tryStatus)
  if(httpStatusCode == 400) stop("400 Bad Request")
  if(httpStatusCode == 401) stop("401 Unauthorized")
  if(httpStatusCode == 403) stop("403 Forbidden")
  if(httpStatusCode == 404) stop("404 Not Found")
  if(httpStatusCode == 500) stop("500 Internal Server Error")
  if(httpStatusCode == 501) stop("501 Not Implemented")
  if(httpStatusCode == 502) stop("502 Bad Gateway")
  if(httpStatusCode == 503) stop("503 Service Unavailable")
  if(httpStatusCode == 504) stop("504 Gateway Timeout")
  if(httpStatusCode >= 400) stop(paste("http response status code",httpStatusCode))
  
  #return response
  responseContent <- content(response)
  if(!is.null(responseContent$errorCode)) stop(responseContent$errorMessage)
  return(responseContent$data)
}


##  M2M Endpoints
##---------------
data-owner <- function(serviceUrl, apiKey, dataOwner){
  # This method is used to provide the contact information of the data owner.
  
  requestUrl <- paste0(serviceUrl,"data-owner")
  payload <- list(dataOwner=dataOwner)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset <- function(serviceUrl, apiKey,
                    datasetId=NULL, datasetName=NULL){
  # This method is used to retrieve the dataset by id or name. 
  
  if(is.null(datasetId) & is.null(datasetName)) stop('Must use either "datasetId" or "datasetName"')
  if(!is.null(datasetId) & !is.null(datasetName)) stop('Must use either "datasetId" or "datasetName"')
  requestUrl <- paste0(serviceUrl,"dataset")
  payload <- list()
  if(!is.null(datasetId)) payload$datasetId <- datasetId
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-bulk-products <- function(serviceUrl, apiKey,
                                  datasetName=NULL){
  # Lists all available bulk products for a dataset - this does not guarantee scene availability.
  
  requestUrl <- paste0(serviceUrl,"dataset-bulk-products")
  payload <- list()
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-catalogs <- function(serviceUrl, apikey){
  # This method is used to retrieve the available dataset catalogs. 
  # The use of dataset catalogs are not required, but are used to group datasets by their use within our web applications. 
  
  requestUrl <- paste0(serviceUrl,"dataset-catalogs")
  payload <- NULL
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-categories <- function(serviceUrl, apikey,
                                 catalog=NULL, includeMessages=NULL, publicOnly=NULL, useCustomization=NULL, parentId=NULL, datasetFilter=NULL){
  # This method is used to search datasets under the categories.   
  
  requestUrl <- paste0(serviceUrl,"dataset-categories")
  payload <- list()
  if(!is.null(catalog)) payload$catalog <- catalog
  if(!is.null(includeMessages)) payload$includeMessages <- includeMessages
  if(!is.null(publicOnly)) payload$publicOnly <- publicOnly
  if(!is.null(useCustomization)) payload$useCustomization <- useCustomization
  if(!is.null(parentId)) payload$parentId <- parentId
  if(!is.null(datasetFilter)) payload$datasetFilter <- datasetFilter
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-clear-customization <- function(serviceUrl, apiKey,
                                        datasetName=NULL, metadataType=NULL){
  # This method is used the remove an entire customization or clear out a specific metadata type. 
  
  requestUrl <- paste0(serviceUrl,"dataset-clear-customization")
  payload <- list()
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-coverage <- function(serviceUrl, apiKey, datasetName){
  # Returns coverage for a given dataset. 
  
  requestUrl <- paste0(serviceUrl,"dataset-coverage")
  payload <- list(datasetName=datasetName)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-download-options <- function(serviceUrl, apiKey, datasetName,
                                      sceneFilter=NULL){
  # This request lists all available products for a given dataset - this does not guarantee scene availability. 
  
  requestUrl <- paste0(serviceUrl,"dataset-download-options")
  payload <- list(datasetName=datasetName)
  if(!is.null(sceneFilter)) payload$sceneFilter <- sceneFilter
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-filters <- function(serviceUrl, apiKey, datasetName){
  # This request is used to return the metadata filter fields for the specified dataset. 
  # These values can be used as additional criteria when submitting search and hit queries. 
  
  requestUrl <- paste0(serviceUrl,"dataset-filters")
  payload <- list(datasetName=datasetName)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-get-customization <- function(serviceUrl, apiKey, datasetName){
  # This method is used to retrieve metadata customization for a specific dataset. 
  
  requestUrl <- paste0(serviceUrl,"dataset-get-customization")
  payload <- list(datasetName=datasetName)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-get-customizations <- function(serviceUrl, apiKey,
                                        datasetNames=NULL, metadataType=NULL){
  # This method is used to retrieve metadata customizations for multiple datasets at once. 
  
  requestUrl <- paste0(serviceUrl,"dataset-get-customizations")
  payload <- list()
  if(!is.null(datasetNames)) payload$datasetNames <- datasetNames
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-messages <- function(serviceUrl, apiKey,
                               catalog=NULL, datasetName=NULL, datasetNames=NULL){
  # Returns any notices regarding the given datasets features. 
  
  requestUrl <- paste0(serviceUrl,"dataset-messages")
  payload <- list()
  if(!is.null(catalog)) payload$catalog <- catalog
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  if(!is.null(datasetNames)) payload$datasetNames <- datasetNames
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-metadata <- function(serviceUrl, apiKey, datasetName){
  # This method is used to retrieve all metadata fields for a given dataset. 
  
  requestUrl <- paste0(serviceUrl,"dataset-metadata")
  payload <- list(datasetName=datasetName)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-order-products <- function(serviceUrl, apiKey, datasetName){
  # Lists all available order products for a dataset - this does not guarantee scene availability. 
  
  requestUrl <- paste0(serviceUrl,"dataset-order-products")
  payload <- list(datasetName=datasetName)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-search <- function(serviceUrl, apiKey,
                             catalog=NULL, categoryId=NULL, datasetName=NULL, includeMessages=NULL, publicOnly=NULL, includeUnknownSpatial=NULL,
                             temporalFilter=NULL, spatialFilter=NULL, sortDirection=NULL, sortField=NULL, useCustomization=NULL){
  # This method is used to find datasets available for searching. 
  # By passing only API Key, all available datasets are returned. 
  # Additional parameters such as temporal range and spatial bounding box can be used to find datasets that provide more specific data. 
  # The dataset name parameter can be used to limit the results based on matching the supplied value against the public dataset name with assumed wildcards at the beginning and end. 
  
  requestUrl <- paste0(serviceUrl,"dataset-search")
  payload <- list()
  if(!is.null(catalog)) payload$catalog <- catalog
  if(!is.null(categoryId)) payload$categoryId <- categoryId
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  if(!is.null(includeMessages)) payload$includeMessages <- includeMessages
  if(!is.null(publicOnly)) payload$publicOnly <- publicOnly
  if(!is.null(includeUnknownSpatial)) payload$includeUnknownSpatial <- includeUnknownSpatial
  if(!is.null(temporalFilter)) payload$temporalFilter <- temporalFilter
  if(!is.null(spatialFilter)) payload$spatialFilter <- spatialFilter
  if(!is.null(sortDirection)) payload$sortDirection <- sortDirection
  if(!is.null(sortField)) payload$sortField <- sortField
  if(!is.null(useCustomization)) payload$useCustomization <- useCustomization
  
  searchResults <- sendRequest(requestUrl, payload, apiKey=apiKey)
}

dataset-set-customization <- function(serviceUrl, apiKey, datasetName,
                                      excluded=NULL, metadata=NULL, searchSort=NULL){
  # This method is used to create or update dataset customizations for a given dataset. 
  
  requestUrl <- paste0(serviceUrl,"dataset-set-customization")
  payload <- list(datasetName=datasetName)
  if(!is.null(excluded)) payload$excluded <- excluded
  if(!is.null(metadata)) payload$metadata <- metadata
  if(!is.null(searchSort)) payload$searchSort <- searchSort
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

dataset-set-customizations <- function(serviceUrl, apiKey, datasetCustomization){
  # This method is used to create or update customizations for multiple datasets at once. 
  
  requestUrl <- paste0(serviceUrl,"dataset-set-customizations")
  payload <- list(datasetCustomization=datasetCustomization)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-complete-proxied <- function(serviceUrl, apiKey, proxiedDownloads){
  # Updates status to 'C' with total downloaded file size for completed proxied downloads 
  
  requestUrl <- paste0(serviceUrl,"download-complete-proxied")
  payload <- list(proxiedDownloads=proxiedDownloads)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-eula <- function(serviceUrl, apiKey,
                          eulaCode=NULL, eulaCodes=NULL){
  # Gets the contents of a EULA from the eulaCodes. 
  
  requestUrl <- paste0(serviceUrl,"download-eula")
  payload <- list()
  if(!is.null(eulaCode)) payload$eulaCode <- eulaCode
  if(!is.null(eulaCodes)) payload$eulaCodes <- eulaCodes
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-labels <- function(serviceUrl, apiKey,
                            downloadApplication=NULL){
  # Gets a list of unique download labels associated with the orders. 
  
  requestUrl <- paste0(serviceUrl,"download-labels")
  payload <- list()
  if(!is.null(downloadApplication)) payload$downloadApplication <- downloadApplication
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-options <- function(serviceUrl, apiKey, datasetName,
                             entityIds=NULL, listId=NULL){
  # The download options request is used to discover downloadable products for each dataset. 
  # If a download is marked as not available, an order must be placed to generate that product. 
  
  requestUrl <- paste0(serviceUrl,"download-options")
  payload <- list(datasetName=datasetName)
  if(!is.null(entityIds)) payload$entityIds <- entityIds
  if(!is.null(listId)) payload$listId <- listId
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-order-load <- function(serviceUrl, apiKey,
                                downloadApplication=NULL, label=NULL){
  # This method is used to prepare a download order for processing by moving the scenes into the queue for processing 
  
  requestUrl <- paste0(serviceUrl,"download-order-load")
  payload <- list()
  if(!is.null(downloadApplication)) payload$downloadApplication <- downloadApplication
  if(!is.null(label)) payload$label <- label
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-order-remove <- function(serviceUrl, apiKey, label,
                                  downloadApplication=NULL){
  # This method is used to remove an order from the download queue. 
  
  requestUrl <- paste0(serviceUrl,"download-order-remove")
  payload <- list(label=label)
  if(!is.null(downloadApplication)) payload$downloadApplication <- downloadApplication
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-remove <- function(serviceUrl, apiKey, downloadId){
  # Removes an item from the download queue. 
  
  requestUrl <- paste0(serviceUrl,"download-remove")
  payload <- list(downloadId=downloadId)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-request <- function(serviceUrl, apiKey,
                             configurationCode=NULL, downloadApplication=NULL, downloads=NULL, dataPaths=NULL, label=NULL, systemId=NULL){
  # This method is used to insert the requested downloads into the download queue and returns the available download URLs.
  
  # Each ID supplied in the downloads parameter you provide will be returned in one of three elements:
  #   * availableDownloads - URLs provided in this list are immediately available; note that these URLs take you to other distribution systems that may require authentication
  #   * preparingDownloads - IDs have been accepted but the URLs are NOT YET available for use
  #   * failed - IDs were rejected; see the errorMessage field for an explanation
  # 
  # Other information is also provided in the response:
  #   * newRecords - Includes a downloadId for each element of the downloads parameter that was accepted and a label that applies to the whole request
  #   * duplicateProducts - Requests that duplicate previous requests by the same user; these are not re-added to the queue and are not included in newRecords
  #   * numInvalidScenes - The number of products that could not be found by ID or failed to be requested for any reason
  # 
  # This API may be online while the distribution systems are unavailable. 
  # When this occurs, you will receive the following error when requesting products that belong to any of these systems:
  #   'This download has been temporarily disabled, please wait at least one hour before requesting'. 
  # Once the distribution system is back online, this error will stop occurring and download requests will succeed. 
  # 
  requestUrl <- paste0(serviceUrl,"download-request")
  payload <- list()
  if(!is.null(configurationCode)) payload$configurationCode <- configurationCode
  if(!is.null(downloadApplication)) payload$downloadApplication <- downloadApplication
  if(!is.null(downloads)) payload$downloads <- downloads
  if(!is.null(dataPaths)) payload$dataPaths <- dataPaths
  if(!is.null(label)) payload$label <- label
  if(!is.null(systemId)) payload$systemId <- systemId
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-retrieve <- function(serviceUrl, apiKey,
                              downloadApplication=NULL, label=NULL){
  # Returns all available and previously requests but not completed downloads.
  # This API may be online while the distribution systems are unavailable. 
  # When this occurs, the downloads being fulfilled by those systems will not appear as available nor are they counted in the 'queueSize' response field. 
  
  requestUrl <- paste0(serviceUrl,"download-retrieve")
  payload <- list()
  if(!is.null(downloadApplication)) payload$downloadApplication <- downloadApplication
  if(!is.null(label)) payload$label <- label
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-search <- function(serviceUrl, apiKey,
                            activeOnly=NULL, label=NULL, downloadApplication=NULL){
  # This method is used to search for downloads within the queue, regardless of status, that match the given label. 
  
  requestUrl <- paste0(serviceUrl,"download-search")
  payload <- list()
  if(!is.null(activeOnly)) payload$activeOnly <- activeOnly
  if(!is.null(label)) payload$label <- label
  if(!is.null(downloadApplication)) payload$downloadApplication <- downloadApplication
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

download-summary <- function(serviceUrl, apiKey, downloadApplication, label,
                             sendEmail=NULL){
  # Gets a summary of all downloads, by dataset, for any matching labels. 
  
  requestUrl <- paste0(serviceUrl,"download-summary")
  payload <- list(downloadApplication=downloadApplication, label=label)
  if(!is.null(sendEmail)) payload$sendEmail <- sendEmail
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

grid2ll <- function(serviceUrl, apiKey, gridType, 
                    responseShape=NULL, path=NULL, row=NULL){
  # Used to translate between known grids and coordinates. 
  
  requestUrl <- paste0(serviceUrl,"grid2ll")
  payload <- list(gridType=gridType)
  if(!is.null(responseShape)) payload$responseShape <- responseShape
  if(!is.null(path)) payload$path <- path
  if(!is.null(row)) payload$row <- row
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

login <- function(serviceUrl, username, password, 
                  userContent=NULL){
  # Upon a successful login, an API key will be returned. 
  # This key will be active for two hours and should be destroyed upon final use of the service by calling the logout method. 
  
  requestUrl <- paste0(serviceUrl,"login")
  payload <- list(username=username, password=password)
  if(!is.null(userContent)) payload$userContent <- userContent
  response <- sendRequest(requestUrl, payload)
  return(response)
}

login-app-guest <- function(serviceUrl, applicationToken, userToken){ #To be checked
  # This endpoint assumes that the calling application has generated a single-use token to complete the authentication and return an API Key specific to that guest user. 
  # All subsequent requests should use the API Key under the 'X-Auth-Token' HTTP header as the Single Sign-On cookie will not authenticate those requests. 
  # The API Key will be active for two hours, which is restarted after each subsequent request, and should be destroyed upon final use of the service by calling the logout method.
  # The 'appToken' field will be used to verify the 'Referrer' HTTP Header to ensure the request was authentically sent from the assumed application. 
  
  requestUrl <- paste0(serviceUrl,"login-app-guest")
  payload <- list(applicationToken=applicationToken, userToken=userToken)
  response <- sendRequest(requestUrl, payload)
  return(response)
}

login-sso <- function(serviceUrl,
                      userContext=NULL){ #To be checked
  # This endpoint assumes that a user has an active ERS Single Sign-On Cookie in their browser or attached to this request. 
  # Authentication will be performed from the Single Sign-On Cookie and return an API Key upon successful authentication. 
  # All subsequent requests should use the API Key under the 'X-Auth-Token' HTTP header as the Single Sign-On cookie will not authenticate those requests. 
  # The API Key will be active for two hours, which is restarted after each subsequent request, and should be destroyed upon final use of the service by calling the logout method. 
  
  requestUrl <- paste0(serviceUrl,"login-sso")
  payload <- list()
  if(!is.null(userContext)) payload$userContext <- userContext
  response <- sendRequest(requestUrl, payload)
  return(response)
}

logout <- function(serviceUrl, apiKey){
  # This method is used to remove the users API key from being used in the future. 
  
  requestUrl <- paste0(serviceUrl,"logout")
  payload <- NULL
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
}

notifications <- function(serviceUrl, apiKey, systemId){
  # Gets a notification list. 
  
  requestUrl <- paste0(serviceUrl,"notifications")
  payload <- list(systemId=systemId)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

order-products <- function(serviceUrl, apiKey, datasetName,
                           entityIds=NULL, listId=NULL){
  # Gets a list of currently selected products - paginated. 
  
  requestUrl <- paste0(serviceUrl,"order-products")
  payload <- list(datasetName=datasetName)
  if(!is.null(entityIds)) payload$entityIds <- entityIds
  if(!is.null(listId)) payload$listId <- listId
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

order-submit <- function(serviceUrl, apiKey, products,
                         autoBulkOrder=NULL, processingParameters=NULL, priority=NULL, orderComment=NULL, systemId=NULL){
  # Submits the current product list as a TRAM order - internally calling tram-order-create. 
  
  requestUrl <- paste0(serviceUrl,"order-submit")
  payload <- list(products=products)
  if(!is.null(autoBulkOrder)) payload$autoBulkOrder <- autoBulkOrder
  if(!is.null(processingParameters)) payload$processingParameters <- processingParameters
  if(!is.null(priority)) payload$priority <- priority
  if(!is.null(orderComment)) payload$orderComment <- orderComment
  if(!is.null(systemId)) payload$systemId <- systemId
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

permissions <- function(serviceUrl, apiKey){
  # Returns a list of user permissions for the authenticated user. This method does not accept any input. 
  
  requestUrl <- paste0(serviceUrl,"permissions")
  payload <- NULL
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-list-add <- function(serviceUrl, apiKey, listId, datasetName,
                           idField=NULL, entityId=NULL, entityIds=NULL, timeToLive=NULL, checkDownloadRestriction=NULL){
  # Adds items in the given scene list. 
  
  requestUrl <- paste0(serviceUrl,"scene-list-add")
  payload <- list(listId=listId, datasetName=datasetName)
  if(!is.null(idField)) payload$idField <- idField
  if(!is.null(entityId)) payload$entityId <- entityId
  if(!is.null(entityIds)) payload$entityIds <- entityIds
  if(!is.null(timeToLive)) payload$timeToLive <- timeToLive
  if(!is.null(checkDownloadRestriction)) payload$checkDownloadRestriction <- checkDownloadRestriction
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-list-get <- function(serviceUrl, apiKey, listId, datasetName=NULL, startingNumber=NULL, maxResults=NULL){
  # Returns items in the given scene list. 
  
  requestUrl <- paste0(serviceUrl,"scene-list-get")
  payload <- list(listId=listId)
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  if(!is.null(startingNumber)) payload$startingNumber <- startingNumber
  if(!is.null(maxResults)) payload$maxResults <- maxResults
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-list-remove <- function(serviceUrl, apiKey, listId,
                              datasetName=NULL, entityId=NULL, entityIds=NULL){
  # Removes items from the given list. 
  # If no datasetName is provided, the call removes the whole list.
  # If a datasetName is provided but no entityId, this call removes that dataset with all its IDs. 
  # If a datasetName and entityId(s) are provided, the call removes the ID(s) from the dataset. 
  
  requestUrl <- paste0(serviceUrl,"scene-list-remove")
  payload <- list(listId=listId)
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  if(!is.null(entityId)) payload$entityId<- entityId
  if(!is.null(entityIds)) payload$entityIds <- entityIds
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-list-summary <- function(serviceUrl, apiKey, listId,
                               datasetName=NULL){
  # Returns summary information for a given list. 
  
  requestUrl <- paste0(serviceUrl,"scene-list-summary")
  payload <- list(listId=listId)
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-list-types <- function(serviceUrl, apiKey,
                             listFilter=NULL){
  # Returns scene list types (exclude, search, order, bulk, etc). 
  
  requestUrl <- paste0(serviceUrl,"scene-list-types")
  payload <- list()
  if(!is.null(listFilter)) payload$listFilter <- listFilter
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-metadata <- function(serviceUrl, apiKey, datasetName, entityId, 
                           idType=NULL, metadataType=NULL, includeNullMetadataValues=NULL, useCustomization=NULL){
  # This request is used to return metadata for a given scene. 
  
  requestUrl <- paste0(serviceUrl,"scene-metadata")
  payload <- list(datasetName=datasetName, entityId=entityId)
  if(!is.null(idType)) payload$idType <- idType
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  if(!is.null(includeNullMetadataValues)) payload$includeNullMetadataValues <- includeNullMetadataValues
  if(!is.null(useCustomization)) payload$useCustomization <- useCustomization
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-metadata-list <- function(serviceUrl, apiKey, listId,
                                datasetName=NULL, metadataType=NULL, includeNullMetadataValues=NULL, useCustomization=NULL){
  # Scene Metadata where the input is a pre-set list. 
  
  requestUrl <- paste0(serviceUrl,"scene-metadata-list")
  payload <- list(listId=listId)
  if(!is.null(datasetName)) payload$datasetName <- datasetName
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  if(!is.null(includeNullMetadataValues)) payload$includeNullMetadataValues <- includeNullMetadataValues
  if(!is.null(useCustomization)) payload$useCustomization <- useCustomization
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-metadata-xml <- function(serviceUrl, apiKey, datasetName, entityId,
                               metadataType=NULL){
  # Returns metadata formatted in XML, ahering to FGDC, ISO and EE scene metadata formatting standards. 
  
  requestUrl <- paste0(serviceUrl,"scene-metadata-xml")
  payload <- list(datasetName=datasetName, entityId=entityId)
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-search <- function(serviceUrl, apiKey, datasetName,
                           maxResults=NULL,startingNumber=NULL,metadataType=NULL, sortField=NULL,sortDirection=NULL,sortCustomization=NULL,
                           useCustomization=NULL,sceneFilter=NULL,compareListName=NULL, bulkListName=NULL,orderListName=NULL,excludeListName=NULL,
                           includeNullMetadataValues=NULL){
  
  # Searching is done with limited search criteria. 
  # All coordinates are assumed decimal-degree format. 
  # If lowerLeft or upperRight are supplied, then both must exist in the request to complete the bounding box. 
  # Starting and ending dates, if supplied, are used as a range to search data based on acquisition dates. 
  # The current implementation will only search at the date level, discarding any time information. 
  # If data in a given dataset is composite data, or data acquired over multiple days, a search will be done to match any intersection of the acquisition range. 
  # There currently is a 50,000 scene limit for the number of results that are returned, however, some client applications may encounter timeouts for large result sets for some datasets. 
  # To use the sceneFilter field, pass one of the four search filter objects (SearchFilterAnd, SearchFilterBetween, SearchFilterOr, SearchFilterValue) in JSON format with sceneFilter being the root element of the object.
  # 
  # The response of this request includes a 'totalHits' response parameter that indicates the total number of scenes that match the search query to allow for paginiation. 
  # Due to this, searches without a 'sceneFilter' parameter can take much longer to execute. 
  # To minimize this impact we use a cached scene count for 'totalHits' instead of computing the actual row count. An additional field, 'totalHitsAccuracy', is also included in the response to indicate if the 'totalHits' value was computed based off the query or using an approximated value. 
  # This does not impact the users ability to access these results via pagination. 
  # This cached value is updated daily for all datasets with active data ingests. 
  # Ingest frequency for each dataset can be found using the 'ingestFrequency' field in the dataset, dataset-categories and dataset-search endpoint responses. 
  
  requestUrl <- paste0(serviceUrl,"scene-search")
  payload <- list(datasetName=datasetName)
  if(!is.null(maxResults)) payload$maxResults <- maxResults
  if(!is.null(startingNumber)) payload$startingNumber <- startingNumber
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  if(!is.null(sortField)) payload$sortField <- sortField
  if(!is.null(sortDirection)) payload$sortDirection <- sortDirection
  if(!is.null(sortCustomization)) payload$sortCustomization <- sortCustomization
  if(!is.null(useCustomization)) payload$useCustomization <- useCustomization
  if(!is.null(sceneFilter)) payload$sceneFilter <- sceneFilter
  if(!is.null(compareListName)) payload$compareListName <- compareListName
  if(!is.null(bulkListName)) payload$bulkListName <- bulkListName
  if(!is.null(orderListName)) payload$orderListName <- orderListName
  if(!is.null(excludeListName)) payload$excludeListName <- excludeListName
  if(!is.null(includeNullMetadataValues)) payload$includeNullMetadataValues <- includeNullMetadataValues
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-search-delete <- function(serviceUrl, apiKey, datasetName,
                                maxResults=NULL, startingNumber=NULL, sortField=NULL, sortDirection=NULL, temporalFilter=NULL){
  # This method is used to detect deleted scenes from datasets that support it. 
  # Supported datasets are determined by the 'supportDeletionSearch' parameter in the 'datasets' response. 
  # There currently is a 50,000 scene limit for the number of results that are returned, however, some client applications may encounter timeouts for large result sets for some datasets.

  requestUrl <- paste0(serviceUrl,"scene-search-delete")
  payload <- list(datasetName=datasetName)
  if(!is.null(maxResults)) payload$maxResults <- maxResults
  if(!is.null(startingNumber)) payload$startingNumber <- startingNumber
  if(!is.null(sortField)) payload$sortField <- sortField
  if(!is.null(sortDirection)) payload$sortDirection <- sortDirection
  if(!is.null(temporalFilter)) payload$temporalFilter <- temporalFilter
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

scene-search-secondary <- function(serviceUrl, apiKey, entityId, datasetName,
                                   maxResults=NULL, startingNumber=NULL, metadataType=NULL, sortField=NULL, sortDirection=NULL,
                                   compareListName=NULL, bulkListName=NULL, orderListName=NULL, excludeListName=NULL){
  # This method is used to find the related scenes for a given scene. 
  
  requestUrl <- paste0(serviceUrl,"scene-search-secondary")
  payload <- list(entityId=entityId, datasetName=datasetName)
  if(!is.null(maxResults)) payload$maxResults <- maxResults
  if(!is.null(startingNumber)) payload$startingNumber <- startingNumber
  if(!is.null(metadataType)) payload$metadataType <- metadataType
  if(!is.null(sortField)) payload$sortField <- sortField
  if(!is.null(sortDirection)) payload$sortDirection <- sortDirection
  if(!is.null(compareListName)) payload$compareListName <- compareListName
  if(!is.null(bulkListName)) payload$bulkListName <- bulkListName
  if(!is.null(orderListName)) payload$orderListName <- orderListName
  if(!is.null(excludeListName)) payload$excludeListName <- excludeListName
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-detail-update <- function(serviceUrl, apiKey, orderNumber, detailKey, detailValue){
  # This method is used to set metadata for an order. 
  
  requestUrl <- paste0(serviceUrl,"tram-order-detail-update")
  payload <- list(orderNumber=orderNumber, detailKey=detailKey, detailValue=detailValue)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-details <- function(serviceUrl, apiKey, orderNumber){
  # This method is used to view the metadata within an order. 
  
  requestUrl <- paste0(serviceUrl,"tram-order-details")
  payload <- list(orderNumber=orderNumber)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-details-clear <- function(serviceUrl, apiKey, orderNumber){
  # This method is used to clear all metadata within an order.  
  
  requestUrl <- paste0(serviceUrl,"tram-order-details-clear")
  payload <- list(orderNumber=orderNumber)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-details-remove <- function(serviceUrl, apiKey, orderNumber, detailKey){
  # This method is used to remove the metadata within an order. 
  
  requestUrl <- paste0(serviceUrl,"tram-order-details-remove")
  payload <- list(orderNumber=orderNumber, detailKey=detailKey)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-search <- function(serviceUrl, apiKey,
                              orderId=NULL, maxResults=NULL, systemId=NULL, sortAsc=NULL, sortField=NULL, statusFilter=NULL){
  # Search TRAM orders. 
  
  requestUrl <- paste0(serviceUrl,"tram-order-search")
  payload <- list()
  if(!is.null(orderId)) payload$orderId <- orderId
  if(!is.null(maxResults)) payload$maxResults <- maxResults
  if(!is.null(systemId)) payload$systemId <- systemId
  if(!is.null(sortAsc)) payload$sortAsc <- sortAsc
  if(!is.null(sortField)) payload$sortField <- sortField
  if(!is.null(statusFilter)) payload$statusFilter <- statusFilter
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-status <- function(serviceUrl, apiKey, orderNumber){
  # Gets the status of a TRAM order. 
  
  requestUrl <- paste0(serviceUrl,"tram-order-status")
  payload <- list(orderNumber=orderNumber)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

tram-order-units <- function(serviceUrl, apiKey, orderNumber){
  # Lists units for a specified order. 
  
  requestUrl <- paste0(serviceUrl,"tram-order-units")
  payload <- list(orderNumber=orderNumber)
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

user-preference-get <- function(serviceUrl, apiKey,
                                systemId=NULL, setting=NULL){
  # (Description Unavailable)
  
  requestUrl <- paste0(serviceUrl,"user-preference-get")
  payload <- list()
  if(!is.null(systemId)) payload$systemId <- systemId
  if(!is.null(setting)) payload$setting <- setting
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

user-preference-set <- function(serviceUrl, apiKey,
                                systemId=NULL, userPreferences=NULL){
  # (Description Unavailable)
  
  requestUrl <- paste0(serviceUrl,"user-preference-set")
  payload <- list()
  if(!is.null(systemId)) payload$systemId <- systemId
  if(!is.null(userPreferences)) payload$userPreferences <- userPreferences
  response <- sendRequest(requestUrl, payload, apiKey=apiKey)
  return(response)
}

# m2m-FunctionName <- function(serviceUrl, apiKey,
#                          parameter1=NULL,
#                          paramter2=NULL){
#   # Method info
#   
#   requestUrl <- paste0(serviceUrl,"servicepoint")
#   payload <- list()
#   if(!is.null(parameter1)) payload$parameter1 <- parameter1
#   if(!is.null(parameter2)) payload$parameter2 <- parameter2
#   
#   response <- sendRequest(requestUrl, payload, apiKey=apiKey)
#   return(response)
# }

##---------------

##  Filters











#Build filters
  ##Scene filter
buildSceneFilter <- function(acquisitionFilter=NULL, cloudCoverFilter=NULL, datasetName=NULL,
                             ingestFilter=NULL, metadataFilter=NULL, seasonalFilter=NULL,
                             spatialFilter=NULL){
  sceneFilter <- list()
  if(!is.null(acquisitionFilter)) sceneFilter$acquisitionFilter <- acquisitionFilter
  if(!is.null(cloudCoverFilter)) sceneFilter$cloudCoverFilter <- cloudCoverFilter
  if(!is.null(datasetName)) sceneFilter$datasetName <- datasetName
  if(!is.null(ingestFilter)) sceneFilter$ingestFilter <- ingestFilter
  if(!is.null(metadataFilter)) sceneFilter$metadataFilter <- metadataFilter
  if(!is.null(seasonalFilter)) sceneFilter$seasonalFilter <- seasonalFilter
  if(!is.null(spatialFilter)) sceneFilter$spatialFilter <- spatialFilter
  
  return(sceneFilter)
}


  ##Acquisition filter
buildAcquisitionFilter <- function(startDate, endDate,
                                   startTime=NULL, endTime=NULL){
  
  startDate <- paste0(substr(startDate,1,4), "-", substr(startDate,5,6), "-", substr(startDate,7,8))
  endDate <- paste0(substr(endDate,1,4), "-", substr(endDate,5,6), "-", substr(endDate,7,8))
  
  if(is.null(startTime)) startDate <- paste0(startDate,"T00:00:00") else startDate <- paste0(startDate,"T",startTime)
  if(is.null(endTime)) endDate <- paste0(endDate,"T23:59:59") else endDate <- paste0(endDate,"T",endTime)
  
  acquisitionFilter <- list(start=startDate, end=endDate)
  if (as.numeric(as.Date(startDate) - as.Date(endDate))>0) warning('"endDate" is before "startDate"')
  return(acquisitionFilter)
}

  ##Cloud cover filter
buildCloudCoverFilter <- function(minCC=0, maxCC=100, includeUnknown=FALSE){
  cloudCoverFilter <- list(min=minCC, max=maxCC, includeUnknown=includeUnknown)
}

  ##Ingest filter
buildIngestFilter <- buildAcquisitionFilter

  ##Metadata filter #TO BE COMPLETED
metaDataFilter <- buildMetadataFilter(filterType,
                                      childFilters=NULL,
                                      ){
  
  if(filterType=="and" | filterType=="or"){
    #do something
    
  } else if(filterType=="between"){
    #do something
    
  } else if(filterType=="value"){
    #do something
    
  } else {
    stop('"filterType" must be one of ["and", "or", "between", "value"]')
  }
  
  
  return(metaDataFilter)
}

  ##Seasonal filter
buildSeasonalFilter <- function(months){
  if (length(months)==1){
    #can't make filter work on single integer, so replicating value. To be improved.
    seasonalFilter <- rep(months,2)
  } else {
    seasonalFilter <- months
  }
}

  ##Spatial filter
buildSpatialFilter <- function(type,
                               llLat=NULL, llLon=NULL, urLat=NULL, urLon=NULL,
                               geoJson=NULL){
  if(type=="mbr"){
    if(is.null(llLat) | is.null(llLon) | is.null(urLat) | is.null(urLon)) stop('In "buildSpatialFilter" - Missing coordinate')
    spatialFilter <- list(filterType=type,
                          lowerLeft=list(latitude=llLat, longitude=llLon),
                          upperRight=list(latitude=urLat, longitude=urLon))
    if(llLat>urLat) warning('Lower left latitude larger than upper right latitude')
    if(llLon>urLon) warning('Lower left longiture larger than upper right longitude')
    
  } else if (type=="geoJson") {
    spatialFilter <- list(filterType=type,
                          geoJson=geoJson)
  } else{
    stop('"type" must be one of "mbr" or "geoJson"')
  }
  return(spatialFilter)
}

  ##Temporal filter
buildTemporalFilter <- buildAcquisitionFilter









