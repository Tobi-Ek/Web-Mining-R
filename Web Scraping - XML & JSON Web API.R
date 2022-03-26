# ******************************************
# Web Scraping - XML & JSON Web API   ######
# ******************************************

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------Packages------------------------------------------------------------

install.packages("XML")
install.packages("RCurl", dependencies = TRUE)


## ------------Part A - XML Retrieval------------------------------------------------------------
library(RCurl)
library(XML)


# *********************************************
#xmlParse retrieves xml data from a url and parses it
df3 <- xmlParse(getURL("http://data.tii.ie/Datasets/Its/DatexII/WeatherData/Content.xml"))

# *********************************************
df4 <- xmlToList(df3)
View(df4)
typeof(df4)
View(df4$payloadPublication)

# *********************************************
df4$payloadPublication$siteMeasurements



## ---------Part B - Json Retrieval---------------------------------------------------------------


# Import library
library(jsonlite)

data_url <- "https://data.nasa.gov/data.json"

# *********************************************
# A very big JSON file
df1 <- jsonlite::fromJSON(data_url)

#Note that this does take a few minutes to load
# Check the structure of the R object
str(df1)

# *********************************************
# Check out some parts of the data
head(df1$dataset$description)
head(df1$dataset$landingPage)
head(df1$dataset$title)
head(df1$dataset$theme) # notice this is an array

# *********************************************
# Make a new data frame with a subset of the data we care about
length(df1$dataset$title)
length(df1$dataset$theme)
length(df1$dataset$description)
length(df1$dataset$landingPage)
mydf1 <- list(title = df1$dataset$title,
                    theme = df1$dataset$theme,
                    description = df1$dataset$description,
                    landingpage = df1$dataset$landingPage
                    )

head(mydf1[[1]])
head(mydf1[[2]])
head(mydf1[[3]])
head(mydf1[[4]])

View(mydf1)


## ---------Part C - Json Retrieval---------------------------------------------------------------


library(jsonlite)

diamond_url <- "http://jeroen.github.io/data/diamonds.json"

# *********************************************
# The stream_in and stream_out functions implement line-by-line processing of JSON data
# over a connection, such as a socket, url, file or pipe.
dmd_url <- stream_in(url(diamond_url))

# Check the structure of the R object
str(dmd_url)
View(dmd_url)

# *********************************************
# Check out some parts of the data
head(dmd_url$carat)
head(dmd_url$cut)

.libPaths()
