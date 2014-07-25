#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Load packages
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
library(shiny)
library(maps)
library(geosphere)
library(xlsx)
library(RColorBrewer)
library(scales)
library(plyr)  
library(ggplot2)
library(sp)
library(rgeos)
library(reshape2)
library(maptools)
source("code/fort.R")
shinyServer(function(input, output) {
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Read and prepare data
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  dataloc <- "data/UN_MigrantStockByOriginAndDestination_2013.xls"
  clist <- "data/countriesun.xlsx"
  countries <- read.xlsx2(clist, sheetName="UN",
                          colClasses=c("character", "character",
                                       "numeric", "numeric"))
  # read world shapefile downloaded from NaturalEarthData.com
  wmap <- readShapeSpatial("data/110m_cultural/ne_110m_admin_0_countries.shp")
  wmap_df <- fortify(wmap)   # convert to dataframe
  rm(wmap)
  # read cities database downloaded from geonames.org
  places <- read.csv("data/cities1000.csv", header=FALSE, stringsAsFactors=FALSE)
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Functions
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  readMigrationTable <- function(xyear = 2013) {
    #usage readMigrationTable()
    #read data for a particular year #2013; 2010; 2008 & 1990
#     if (xyear == 2013) {sheetName <- "Table 10"} else
#     {if (xyear == 2010) {sheetName <- "Table 7"} else
#       {if (xyear == 2000) {sheetName <- "Table 4"} else
#         {if (xyear == 1990) {sheetName <- "Table 1"}
#     }}}
    data2013 <- read.xlsx2(dataloc, sheetName = "Table 10", startRow = 16,
                       colIndex = c(2, 4 , 10:241),
                       colClasses = c("character", rep("numeric", 232))) #read excel sheet selected columns and rows
    #add datayear column
    data2013$datayear <- 2013
    data2010 <- read.xlsx2(dataloc, sheetName = "Table 7", startRow = 16,
                           colIndex = c(2, 4 , 10:241),
                           colClasses = c("character", rep("numeric", 232)))
    data2010$datayear <- 2010
    data2000 <- read.xlsx2(dataloc, sheetName = "Table 4", startRow = 16,
                           colIndex = c(2, 4 , 10:241),
                           colClasses = c("character", rep("numeric", 232)))
    data2000$datayear <- 2000
    data1990 <- read.xlsx2(dataloc, sheetName = "Table 1", startRow = 16,
                           colIndex = c(2, 4 , 10:241),
                           colClasses = c("character", rep("numeric", 232)))
    data1990$datayear <- 1990
    data <- rbind(data2013, data2010, data2000, data1990)
    return(data)
  }
  ## match with country codes
  getCountryCode <- function(xcountry="Nepal") {
    #usage getCountryCode("Australia") will return "AU"
    code <- countries[countries$newname == xcountry,c("ISOCODE")]
    if (is.na(code[1])) {
      return(NA)
    } else {
      return(as.character(code[1]))
    }
  }
  ## match with country codes
  getCountryName <- function(xcountry="NP") {
    #usage getCountryCode("AU") will return "Australia"
    code <- countries[countries$ISOCODE == xcountry, c("COUNTRY_UN")]
    if (is.na(code[1])) {
      return(NA)
    } else {
      return(as.character(code[1]))
    }
  }
  ## function gets the required locations of cities in the required country
  getRandomCity <- function(xcountry = "AU", xnum=1) {
    #xcountry <- "XP"
    allCities <- places.df[places.df$code == xcountry,]
    if (nrow(allCities) == 0) {return(data.frame(lon=rep(NA, xnum),
                                                 lat=rep(NA, xnum),
                                                 code=rep("XY", xnum)))}
    selection <- sample(c(1:nrow(allCities)), xnum, replace=TRUE)
    return(allCities[selection,])
  }
  getRandomCity2 <- function(xcountry = "AU", xnum=1) {
    #selects city based on weighted probality
    allCities <- places.df[places.df$code == xcountry,]
    allCities <- allCities[order(allCities$pop),] #sort
    if (nrow(allCities) == 0) {return(data.frame(lon=rep(NA, xnum),
                                                 lat=rep(NA, xnum),
                                                 code=rep("XY", xnum)))}
    selection <- sample(c(1:nrow(allCities)), xnum, replace=TRUE, prob=allCities$pop)
    return(allCities[selection,])
  }
  ##usage
  # getRandomCity("AE", 4);
  # aep <- getRandomCity2("AE", 400)
  # hist(aep$pop)
  #gets 4 cities' lat long from United Arab Emirates
  getOutMigrants <- function(xcountry = "AU", xyear = 2013) {
    inmig <- sum(m2013[m2013$variable == xcountry &
                         m2013$datayear == xyear,]$STOCK)
    return(inmig)
  }
  getInMigrants <- function(xcountry = "AU", xyear = 2013) {
    outmig <- sum(m2013[m2013$ISOCODE == xcountry &
                          m2013$datayear == xyear,]$STOCK)
    return(outmig)
  }
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Clean UN data
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  data2013 <- readMigrationTable() #not only fro 2013 though
  #data2013 <- data2013[data2013$datayear == {input$country}, ]
  data2013 <- data2013[data2013$Country.code < 900,] #isolate countries only
  countries$newname <- chartr("'", " ", countries$COUNTRY_UN)
  countries$newname <- chartr("(", " ", countries$newname)
  countries$newname <- chartr(")", " ", countries$newname)
  countries$newname <- chartr("-", " ", countries$newname)
  countries$newname <- gsub("\\s","", chartr(",", " ", countries$newname))
  #convert col names to country ISCOCODEs
  oldnames <- names(data2013)
  newnames <- chartr(".", " ", oldnames) #replace . with space
  newnames <- gsub("\\s","", newnames) #final names to match
  countries$ISOCODE <- as.character(countries$ISOCODE)
  newnames2 <- sapply(newnames, getCountryCode)
  newnames2[is.na(newnames2)] <- oldnames[is.na(newnames2)]
  names(data2013) <- newnames2
  data2013$newname <- chartr("'", " ", data2013[,1])
  data2013$newname <- chartr("(", " ", data2013$newname)
  data2013$newname <- chartr(")", " ", data2013$newname)
  data2013$newname <- chartr("-", " ", data2013$newname)
  data2013$newname <- gsub("\\s","", chartr(",", " ", data2013$newname))
  data2013$ISOCODE <- sapply(data2013$newname, getCountryCode)
  #melt data
  data2013.sub <- data2013[,c(-1,-2)]
  m2013 <- melt(data2013.sub, c("newname", "ISOCODE", "datayear"),
                names(data2013)[3:234],
                value.name = "STOCK")
  names(m2013) <- c("newname", "ISOCODE", "datayear", "variable", "STOCK")
rm(data2013)
rm(data2013.sub)
  # head(m2013,12); tail(m2013)
  #force na
  m2013[m2013 == "NaN"] = NA
  m2013[m2013 == ""] = NA
  m2013 <- m2013[!is.na(m2013$STOCK),]
  #force numeric
  m2013$STOCK <- as.numeric(m2013$STOCK)
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  #*     Merge with country details
  #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
m2013.merged <- merge(m2013, countries, by="ISOCODE", all.x=FALSE)
m2013.merged <- merge(m2013.merged, countries, by.x="variable", by.y="ISOCODE", all.x=TRUE)
m2013.merged <- m2013.merged[,c(1,2,4,5,7,8,11,12)]
names(m2013.merged) <- c("source", "destination", "datayear","stock", "lat.d", "lon.d", "lat.s", "lon.s")
m2013.merged$stocklog <- log(m2013.merged$stock)
  places.df <- data.frame (as.numeric(places$V6), as.numeric(places$V5),
                           places$V9, as.numeric(places$V15), places$V2)
rm(places)
  names(places.df) <- c('lon', "lat", "code", "pop", "name")
  places.df <- places.df[,c(1:4)]
  places.df <- places.df[complete.cases(places.df),]
  ##merge regions to match UN data e.g. migrant data in Taiwan (TW) is included
  ##  in China (CN)'s data
  places.df$code[places.df$code == "TW"] <- "CN"
  #remove places with <1000 population
  places.df <- places.df[places.df$pop > 1000,]
  #get top 15 cities only
  places.sub <- ddply(places.df, c("code"), 
                      function(xdf) {
                        rank <- rank(-xdf$pop, ties.method="first")
                        data.frame(cbind(xdf, rank))
                      })
  places.df <- places.sub[places.sub$rank < 16,]
  places.df <- places.df[complete.cases(places.df),]
  #plot vars [default theme - light]
  source.couleur <- "green4" #"green4"
  destination.couleur <- "red1" #"red3"
  mid.couleur <- "steelblue4"
  backdrop.couleur <- "azure2" #"grey4"
  outline.couleur <- "black"  #"slategrey" "honeydew4
  landmass.couleur <- "gray95"
  text.couleur <- "black"
  mapLabel <- "Note: Migration data from United Nations, Department of Economic and Social Affairs, \nPopulation Division (2013). Trends in International Migrant Stock: Migrants by \nDestination and Origin (United Nations database, POP/DB/MIG/Stock/Rev.2013). \nWorld map shapefile from NautralEarth.com. Location and population of cities from geonames.org"
  mapTitle <- "International migration flows from"
  alpha <- 0.4 #0.3 0.2
  size <- 0.05 #0.02 0.01
  legend.emplacement <- c(.12,.22)
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Things are a bit reactive down here
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
myCountries <- reactive({input$country}) #transfer selected country code
myYear <- reactive({input$radio}) 
myTheme <- reactive({input$maptheme})
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Summary data output
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
output$oid1 <- renderPrint(paste("Migrant stock data for",
                                 getCountryName(myCountries()),
{input$radio}))
output$oid2 <- renderPrint(paste("In-migrants",
                                 formatC(getInMigrants(myCountries(),
                                                       as.numeric(myYear())),
                                         format="d", big.mark=",")))
output$oid3 <- renderPrint(paste("Out-migrants",
                                 formatC(getOutMigrants(myCountries(),
                                                        myYear()),
format="d", big.mark=",")))

  output$mapPlot <- renderPlot({
#     myCountries <- input$country #transfer selected country code
#     myYear <- input$radio
#     myTheme <- input$maptheme #get selected theme name
    #     mapTitle <- "International migration flows"
    if (myTheme() == "Dark") {
      source.couleur <- "green1" #"green4"
      destination.couleur <- "red" #"red3"
      mid.couleur <- "skyblue"  #"steelblue4"
      backdrop.couleur <- "black" #"grey4"
      outline.couleur <- "slategrey"  #"slategrey" "honeydew4
      landmass.couleur <- "grey4"
      text.couleur <- "honeydew1"
    }
    ##compute migrant stock figures
    m2013.merged.sub <- m2013.merged[m2013.merged$datayear == myYear() &
                                       (m2013.merged$destination == myCountries() |
                                       m2013.merged$source == myCountries()),]
    m2013.merged.sub <- m2013.merged.sub[m2013.merged.sub$stock > 0,]
    m2013.merged.sub$stocklog <- round(m2013.merged.sub$stocklog,0)
    m2013.merged.sub$source <- as.character(m2013.merged.sub$source)
    #inflate the table to include individual cases
    m2013.merged.sub$id <- c(1:nrow(m2013.merged.sub))
    m2013.merged.sub$source <- as.character(m2013.merged.sub$source)
    m2013.final <- ddply(m2013.merged.sub, c("id"), function(ydf) {
      if (ydf$stocklog == 1) {data <- ydf} else {
        data <- ydf
        for (i in 2: ydf$stocklog) {
          data <- rbind(data, ydf)
        }
        return(data)
        #return(replaceCities(data))
      }
    })
    #replace cities for-loop
    zdf <- m2013.final[,c(-3)] #remove datayear col
    zdf <- zdf[order(zdf$source),]
    source.replace <- ddply(zdf, c("source"), function(xdf) {
      return(data.frame(getRandomCity2(xdf$source[1], nrow(xdf))))
    })
    names(source.replace) <- c("source", "lon.ss", "lat.ss", "code", "pop", "rank")
    nrow(source.replace) - nrow(zdf) #should be 0 to work :)
    zdf <- cbind(zdf, source.replace[,c("lon.ss", "lat.ss")])
    # 2> sort according to destination
    zdf <- zdf[order(zdf$destination),] #sort
    destination.replace <- ddply(zdf, c("destination"), function(xdf) {
      return(data.frame(getRandomCity2(xdf$destination[1], nrow(xdf))))
    })
    names(destination.replace) <- c("destination", "lon.dd", "lat.dd",
                                    "code", "pop", "rank")
    nrow(destination.replace) - nrow(zdf) #should be 0 to work :)
    zdf <- cbind(zdf, destination.replace[,c("lon.dd", "lat.dd")])
    zdf <- zdf[complete.cases(zdf),]
    zdf <- zdf[order(zdf$stocklog),]
    #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    #*     Process for great circle map
    #* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    geosource <- matrix(c(zdf$lon.ss, zdf$lat.ss), ncol=2)
    geodestination <- matrix(c(zdf$lon.dd, zdf$lat.dd), ncol=2)
    arc.nombre <- 80
    cgc <- gcIntermediate(geosource, geodestination, arc.nombre,
                          breakAtDateLine = TRUE, addStartEnd = TRUE, sp = TRUE)
    cgc.ff <- fortify.SpatialLinesDataFrame(cgc)
    
    ggplot() +
      geom_polygon(aes(long,lat,group=group), 
                   size = 0.2, fill=landmass.couleur,
                   colour = NA,
                   data=wmap_df) + #landmass backdrop
      geom_polygon(aes(long,lat,group=group), 
                   size = 0.04, fill=NA,
                   colour = outline.couleur,
                   data=wmap_df, alpha=0.5) + #country boundary
      geom_line(aes(long, lat, group=group, col=order),
                data=cgc.ff, alpha = alpha,
                size= size) + #drawing great circle lines works .02,.03
      scale_colour_gradient2(high=destination.couleur,
                             low=source.couleur,
                             mid=mid.couleur, midpoint=arc.nombre/2,
                             name="Flow legend",
                             labels = c("Migrant origin",
                                        "Migrant destination"),
                             breaks=c(1,arc.nombre),
                             guide="legend") +
      guides(alpha = "none") +
      theme(plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill=backdrop.couleur,
                                            colour=backdrop.couleur),
            legend.position = legend.emplacement,
            legend.background = element_rect(fill = NA,
                                             color=landmass.couleur),
            legend.text = element_text(size = 7, colour = text.couleur),
            legend.title = element_text(size = 8, colour = text.couleur),
            axis.text.x  = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks  = element_blank(),
            axis.title  = element_blank(),
            axis.title  = element_blank()
      ) +
      coord_equal() +
      geom_text(aes(x= 0, y=90, 
                    label="International migration flows"),
                color=text.couleur, size=5)
    #summary
  })
output$outSummary <- renderTable({
#   myCountries <- input$country #transfer selected country code
#   myYear <- input$radio
  m2013.merged.sub <- m2013.merged[m2013.merged$datayear == myYear() &
                                     (m2013.merged$destination == myCountries() |
                                        m2013.merged$source == myCountries()),]
  m2013.merged.sub <- m2013.merged.sub[m2013.merged.sub$stock > 0,]
  m2013.merged.sub$source <- as.character(m2013.merged.sub$source)
  sumOut <- m2013.merged.sub[m2013.merged.sub$source == myCountries(),]
  sumOut <- sumOut[complete.cases(sumOut),]
  sumOut <- sumOut[order(-sumOut$stock),c(2,4)]
  sumOut$destinationname <- sapply(sumOut$destination, getCountryName)
  sumOut <- data.frame("Destination" = sumOut$destinationname, "Migrants" = sumOut$stock)
  head(sumOut,input$summaryRows)
})
output$inSummary <- renderTable({  
  m2013.merged.sub <- m2013.merged[m2013.merged$datayear == myYear() &
                                     (m2013.merged$destination == myCountries() |
                                        m2013.merged$source == myCountries()),]
  m2013.merged.sub <- m2013.merged.sub[m2013.merged.sub$stock > 0,]
  m2013.merged.sub$source <- as.character(m2013.merged.sub$source)
  sumIn <- m2013.merged.sub[m2013.merged.sub$destination == myCountries(),]
  sumIn <- sumIn[complete.cases(sumIn),]
  sumIn <- sumIn[order(-sumIn$stock),c(1,4)]
  sumIn$sourcename <- sapply(sumIn$source, getCountryName)
  sumIn <- data.frame("Origin" = sumIn$sourcename, "Migrants" = sumIn$stock)
  head(sumIn,input$summaryRows)
})
})