
##  Example code
## 

#get the API url
url <- m2mServiceUrl()
#login and get api key (USGS username and password stored in .Renviron)
apiKey <- login(url, Sys.getenv("USGS_USR"), Sys.getenv("USGS_PWD"))
#check my permissions
unlist(permissions(url, apiKey))

#search datasets (want to know which Landsat datasets exist)
landsat_datasets <- dataset_search(url, apiKey,
                              catalog="EE",
                              datasetName = "landsat")
#show the collection names
sapply(landsat_datasets, function(x){x$collectionLongName})
#We want to search Landsat8, collection 2/level 2 images, which is the 30th dataset here ()
L8Dataset <- landsat_datasets[[30]]
L8alias <- L8Dataset$datasetAlias

#Some checks on the data:
  #Info on data owner
do.call(rbind, data_owner(url, apiKey, L8Dataset$dataOwner))
  #You can inspect optional metadata filters for this dataset (not used in later examples)
L8Filters <- dataset_filters(url, apiKey, L8alias)
sapply(L8Filters, function(x){x$fieldLabel})
  #Any messages related to the dataset?
dataset_messages(url, apiKey, datasetName = L8alias)



# Search scenes:
## Create a scene filter with user-defined parameters
mySceneFilter <- SceneFilter(spatialFilter = SpatialFilter("mbr", Coordinate(50, 4), Coordinate(51, 5)),
                             acquisitionFilter = AcquisitionFilter(20200105, 20200831),
                             cloudCoverFilter = CloudCoverFilter(0,60))
L8Scenes <- scene_search(url, apiKey, L8alias, maxResults=100, sceneFilter = mySceneFilter) 
 
#Total number of found scenes (can be larger than "maxResults" parameter)
L8Scenes$totalHits
#We found several scenes !
#List the scene IDs
L8SceneIds <- sapply(L8Scenes$results, function(x){x$entityId})

#Download options
L8DownloadOpt <- download_options(url, apiKey, datasetName=L8alias, entityIds = L8SceneIds)
# View(do.call(rbind,lapply(L8DownloadOpt, function(x){unlist(x[1:10])})))

#Keep the entity and product Id's for the available products
downloads <- lapply(L8DownloadOpt, function(x){if(x$available==TRUE) return(list(entityId=x$entityId, productId=x$id)) else return(NULL)})
downloads <- downloads[-which(sapply(downloads, is.null))]

 






# Logout:
logout(url, apiKey)

