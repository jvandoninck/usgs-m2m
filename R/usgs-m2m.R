##  Version 3.0

##  JSON search and download images from USGS/EROS EarthExplorer Inventory
##    UPDATED 


##



queryEE <- function(userName, pwd, xMin, xMax, yMin, yMax, searchFrom, searchTo,
                        dataSets=c("LANDSAT_TM_C1", "LANDSAT_ETM_C1", "LANDSAT_8_C1"),
                        months=NULL,
                        includeUnknownCloudCover=FALSE,
                        minCloudCover=NULL,
                        maxCloudCover=NULL,
                        maxResults=5000,
                        additionalCriteria=list("Collection Category"="T1", "Data Type Level-1"="L1TP"),
                        setVerbose=TRUE){

  if (setVerbose){
    cat("*******************************\n")
    cat("**  EarthExplorer Query      **\n")
    cat("*******************************\n")
  }
  ##  Load libraries
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(jsonlite))
  
  serviceURL <- 'https://earthexplorer.usgs.gov/inventory/json/v/1.4.0/'

  ##  Request API key
  if (setVerbose) cat("Login \n")
  req <- POST(paste0(serviceURL,'login'), 
              body = URLencode(paste0('jsonRequest={"username":"', userName, '","password":"', pwd, '","authType":"EROS","catalogId":"EE"}')),
              content_type("application/x-www-form-urlencoded; charset=UTF-8"))
  stop_for_status(req, "connect to server.")
  warn_for_status(req)
  responseContent <- content(req)
  if (!is.null(responseContent$errorCode)){
    if (setVerbose) cat(paste0("Authentication error: ",responseContent$error,"\n"))
    return(NULL)
  }
  apiKey <- responseContent$data
  if (setVerbose) cat("Login succeeded\n")
  
  ##  Format (optional) search parameters  
  ##  Parameter "temporalFilter"
  param_temporalFilter <- paste0(
    '"temporalFilter":{"startDate":"',
    paste0(substr(searchFrom,1,4),"-",substr(searchFrom,5,6),"-",substr(searchFrom,7,8),"T00:00:00"),
    '","endDate":"',
    paste0(substr(searchTo,1,4),"-",substr(searchTo,5,6),"-",substr(searchTo,7,8),"T23:59:59"),
    '"},')
  
  ##  Parameter "months"
  if (is.null(months)) 
    param_months <- NULL else
      param_months <- paste0('"months":','[',paste(months, collapse=","),'],')

  ##  Parameter "includeUnknownCloudCover"
  if (includeUnknownCloudCover) 
    param_includeUnknownCloudCover <- '"includeUnknownCloudCover":true,' else
      param_includeUnknownCloudCover <- '"includeUnknownCloudCover":false,'
  
  ##  Parameter "minCloudCover" and "maxCloudCover"
  if (is.null(minCloudCover)) 
    param_minCloudCover <- NULL else
      param_minCloudCover <- paste0('"minCloudCover":"',minCloudCover,'",')
  if (is.null(maxCloudCover))
    param_maxCloudCover <- NULL else
      param_maxCloudCover <- paste0('"maxCloudCover":"',maxCloudCover,'",')
    
  ##  Parameter "additionalCriteria" field ID's
  fieldIDs <- matrix(data=c(19872, 19891, 20520,"=",
                            19873, 19884, 20514,"=",
                            19879, 19887, 20516,"=",
                            19881, 19893, 20522,"<",
                            19874, 19883, 20515,"<",
                            19880, 19890, 20510,"=",
                            19876, 19886, 20517,"="),
                     ncol=4, byrow=TRUE)
  colnames(fieldIDs) <- c("LANDSAT_TM_C1", 
                          "LANDSAT_ETM_C1", 
                          "LANDSAT_8_C1", 
                          "operand")
  rownames(fieldIDs) <- c("Landsat Product Identifier",
                          "WRS Path", 
                          "WRS Row", 
                          "Land Cloud Cover", 
                          "Scene Cloud Cover", 
                          "Collection Category", 
                          "Data Type Level-1")

  allReturn <- NULL
  allSpFootprint <- NULL
  for (dataSet in dataSets){
    if (setVerbose) cat(paste0(dataSet)," ... ")

    ##  Parameter "additionalCriteria"
    if (is.null(additionalCriteria)){
      param_additionalCriteria <- NULL
    } else {
      param_additionalCriteria  <- '"additionalCriteria":{"filterType":"and","childFilters":['
      filt <- NULL
      for (i in 1:length(additionalCriteria)){
        crit <- names(additionalCriteria)[i]
        filt <- c(filt,paste0('{"filterType":"value",',
                               '"fieldId":"',fieldIDs[crit,dataSet],'",',
                               '"value":"',additionalCriteria[[i]],'",',
                               '"operand":"',fieldIDs[crit,"operand"],'"}'))
      } 
      param_additionalCriteria <- paste0(param_additionalCriteria,paste(filt,collapse=","),']},')
    }

    ##  Query
    requestURL <- GET(paste0(serviceURL,
                             'search?jsonRequest={',
                             '"datasetName":"',dataSet,'",',
                             '"spatialFilter":{"filterType":"mbr",',
                               '"lowerLeft":{"latitude":"',yMin,'","longitude":"',xMin,'"},',
                               '"upperRight":{"latitude":"',yMax,'","longitude":"',xMax,'"}},',
                             param_temporalFilter,
                             param_months,
                             param_includeUnknownCloudCover,
                             param_minCloudCover,
                             param_maxCloudCover,
                             '"maxResults":"',maxResults,'",',
                             '"sortOrder":"ASC",',
                             param_additionalCriteria,
                             '"apiKey":"',apiKey,'",',
                             '"node":"EE"}'))
    responseContent <- fromJSON(content(requestURL, "text", encoding="UTF-8"))
    if (setVerbose) cat(responseContent$data$totalHits,"results \n")
    if(responseContent$data$totalHits>0) {
      dataSetReturn <- responseContent$data$results
      allReturn <- rbind(allReturn, dataSetReturn[names(dataSetReturn)!="spatialFootprint"])
      allSpFootprint <- rbind(allSpFootprint, dataSetReturn$spatialFootprint)
    }
  }
  allReturn$spatialFootprint <- allSpFootprint
  
  ##  Logout
  requestURL <- paste0(serviceURL,'logout?jsonRequest={"apiKey":"',apiKey,'"}')
  responseContent <- fromJSON(requestURL)
  
  return(allReturn)
}


downloadEE <- function(userName, pwd, entityIDs, displayIDs,
                       downloadDir=tempdir(),
                       method="auto",
                       setVerbose=TRUE){
  
  if (setVerbose){
    cat("*******************************\n")
    cat("**  EarthExplorer Downnload  **\n")
    cat("*******************************\n")
  }
  ##  Load libraries
  suppressPackageStartupMessages(require(httr))
  suppressPackageStartupMessages(require(jsonlite))
  
  serviceURL <- 'https://earthexplorer.usgs.gov/inventory/json/v/1.4.0/'
  
  ##  Request API key
  if (setVerbose) cat("Login \n")
  req <- POST(paste0(serviceURL,'login'), 
              body = URLencode(paste0('jsonRequest={"username":"', userName, '","password":"', pwd, '","authType":"EROS","catalogId":"EE"}')),
              content_type("application/x-www-form-urlencoded; charset=UTF-8"))
  stop_for_status(req, "connect to server.")
  warn_for_status(req)
  responseContent <- content(req)
  if (!is.null(responseContent$errorCode)){
    if (setVerbose) cat(paste0("Authentication error: ",responseContent$error,"\n"))
    return(NULL)
  }
  apiKey <- responseContent$data

  ##  Download data
  if (setVerbose) cat(paste0("Downloading ",length(entityIDs)," files:\n"))
  if (!file.exists(downloadDir)) dir.create(downloadDir, recursive=TRUE)
  
  outRet <- numeric(length(entityIDs))+NA
  for (s in 1:length(entityIDs)){
    if (setVerbose) cat(paste0("  ",displayIDs[s]," "))
    
    dataSet <- switch(substr(displayIDs[s],1,4),
                      "LT05"="LANDSAT_TM_C1",
                      "LT04"="LANDSAT_TM_C1",
                      "LE07"="LANDSAT_ETM_C1",
                      "LC08"="LANDSAT_8_C1",
                      "LO08"="LANDSAT_8_C1")
    
    requestURL <- GET(paste0(serviceURL,
                             'download?jsonRequest={',
                             '"datasetName":"',dataSet,'",',
                             '"products":["STANDARD"],',
                             '"entityIds":["',entityIDs[s],'"],',
                             '"apiKey":"',apiKey,'","stage":false}'))
    responseContent <- fromJSON(content(requestURL, "text", encoding="UTF-8"))

    if (!is.null(responseContent$errorCode)){
      if (setVerbose) cat("    Error retrieving download url, skipping\n")
      outRet[s] <- 1
    } else {
      if (setVerbose) cat(" ... ")
      downURL <- responseContent$data$url
      fn.out <- file.path(downloadDir,paste0(displayIDs[s],".tar.gz"))
      ptd <- proc.time()
      ret <- download.file(downURL, fn.out, method="auto", quiet = TRUE)
      ptd <- proc.time()-ptd
      if (ret>0 & setVerbose) cat("    Error downloading file, skipping\n")
      if (ret==0 & setVerbose) cat(" Completed: ",round(unname(ptd[3]),0)," sec (",round((file.size(fn.out)/1048576)/unname(ptd[3]),2),"MB/sec) \n", sep="")
      outRet[s] <- ret
    }
  }
  ##  Logout
  requestURL <- paste0(serviceURL,'logout?jsonRequest={"apiKey":"',apiKey,'"}')
  responseContent <- fromJSON(requestURL)

  ##  Return code for successfull completion
  if (setVerbose) cat("**  Download completed  **\n")
  return(outRet)
}

