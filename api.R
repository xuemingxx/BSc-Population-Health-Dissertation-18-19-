library(httr)
library(jsonlite)

API_BASE_URL <- "http://api.ratings.food.gov.uk/"
HEADERS <- c("x-api-version" = 2, "accept" = "application/json")

getRequest <- function(api, type = "parsed") {
  addr <- paste(API_BASE_URL, api, sep="")
  ret <- GET(addr, add_headers(HEADERS))
  if (ret$status_code != 200) {
    print(paste("Request", addr, "failed!"))
  }
  return (content(ret, type))
}

getTargetCountryID <- function(countryName) {
  countries <- getRequest("Countries")$countries
  for (country in countries) {
    if (country$name == countryName) {
      return (country$id)
    }
  }
  print(paste("No country named", countryName, "is found!"))
}

getBusinessTypeID <- function(businessTypeName) {
  businessTypes <- getRequest("BusinessTypes")$businessTypes
  for (businessType in businessTypes) {
    if (businessType$BusinessTypeName == businessTypeName) {
      return (businessType$BusinessTypeId)
    }
  }
  print(paste("No BusinessType named", businessTypeName, "is found!"))
}

uniformEstablishmentsData <- function(data) {
  data <- data[1]$establishments
  data$Hygiene <- data$scores$Hygiene
  data$Structural <- data$scores$Structural
  data$ConfidenceInManagement <- data$scores$ConfidenceInManagement
  data$longitude <- data$geocode$longitude
  data$latitude <- data$geocode$latitude
  data <- subset(data, select = -c(scores, geocode, meta, links))
}

countryID <- getTargetCountryID("England")
businessTypeID <- getBusinessTypeID("Takeaway/sandwich shop")

establishments_data <- data.frame()
# a dummy request to find the total data count
counts <- getRequest(paste("Establishments?countryId=", countryID, "&businessTypeId=", 
                           businessTypeID, "&pageNumber=1&pageSize=10", sep = ""))$meta$totalCount
print(paste(counts, "data in total"))
print("processing...")
pb <- txtProgressBar(min = 0, max = counts, style = 3)
size_per_page <- 5000
current_page_number <- 1
current_count <- 0
while (current_count < counts) {
  request_ret <- getRequest(paste("Establishments?countryId=", countryID, 
                                      "&businessTypeId=", businessTypeID,
                                      "&pageNumber=", current_page_number,
                                      "&pageSize=", size_per_page, sep = ""), "text")
  establishments_data <- rbind(establishments_data, uniformEstablishmentsData(fromJSON(request_ret)))
  current_page_number <- current_page_number + 1
  current_count <- current_count + size_per_page
  setTxtProgressBar(pb, current_count)
}
setTxtProgressBar(pb, counts)