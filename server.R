# server.R
library(ggplot2)
library(grid)
library(stringr)
library(leaflet)

fileType <-  read.csv(file = "fileType.csv", header = TRUE, sep = ",")

load("./walking.RData")

parseFile <- function(fileName, shortName, directory, timestamp) {
   filename <-
      paste(c(directory, "/", as.character(fileName), timestamp, ".csv"),
            collapse = "")
   result = NULL
   str(filename)
   if (file.exists(filename)) {
      result <- read.table(filename,  sep = ",", header = F)
      names(result) <- c("Date", "Time", as.character(shortName))
      result$Timestamp <-
         strptime(paste(as.Date(result$Date), result$Time), "%Y-%m-%d %H:%M:%S")
   }
   str(head(result))
   return(result)
}

parseAllFiles <- function(directory) {
   result <- list()
   timestamp <- list.files(path = directory, full.names = F)
   str(timestamp[1])
   l <- nchar(timestamp[1])
   timestamp <- substr(timestamp[1], l - 22, l - 4)
   for (i in 1:nrow(fileType)) {
      row <- fileType[i, ]
      temp <-
         parseFile(row$FileName, row$FileName, directory, timestamp)
      result[[as.character(row$FileName)]] <- temp
   }
   return(result)
   
   
}

files <-
   data.frame(
      dir = c(
         "./data/SenseView sensor data: 02_04_2016 09:02.zip",
         "./data/SenseView sensor data: 03_04_2016 08:28.zip"
      ),
      date = c(
         "2016-04-02-09-02-29",
         "2016-04-03-08-28-28"
      )
   )

directory <- as.character(files[1,]$dir)
#timestamp <- as.character(files[5, ]$date)

walking <-  parseAllFiles(directory)
str(walking)
save(walking, file = "./walking.RData")

server <- function(input, output, session) {
   values <- reactiveValues(starting = TRUE)
   session$onFlushed(function() {
      values$starting <- FALSE
   })
   
   observeEvent(input$updatebutton, {
      inFile <- input$newfile
      str(inFile)
      if (is.null(inFile))
         return(NULL)
      # unzip it in data folder
      unzip(inFile$datapath, exdir = paste0("./data/", inFile$name))
      # set the label and select items
      updateSelectInput(
         session,
         "dataset",
         label = paste("Choose dataset"),
         choices = list.dirs(path = "./Data", full.names = F),
         selected = inFile$name
      )
   })
   
   observeEvent(input$dataset, {
      walking <-  parseAllFiles(paste0("./data/", input$dataset))
      str(walking)
      save(walking, file = "./walking.RData")
   })
   
   output$dateBox <- renderInfoBox({
      infoBox("Activity date",
              paste0(min(walking$LEHHRMHR$Timestamp)),
              icon = icon("time"),
              color = "green")
   })
   
   output$speedBox <- renderInfoBox({
      infoBox(
         "Speed",
         paste0(
            "Min ",
            min(walking$ANDGPSSP$ANDGPSSP),
            " Max ",
            signif(max(walking$ANDGPSSP$ANDGPSSP), digits = 2),
            "km/h"
         ),
         icon = icon("road"),
         color = "yellow"
      )
   })
   
   output$pulseBox <- renderInfoBox({
      infoBox(
         "Pulse",
         paste0(
            "Min ",
            min(walking$LEHHRMHR$LEHHRMHR),
            " Max ",
            max(walking$LEHHRMHR$LEHHRMHR)
         ),
         icon = icon("heart"),
         color = "red"
      )
   })
   
   output$p1 <- renderPlot({
      input$dataset
      if (values$starting)
         return(NULL)
      print(walking)#$LEHHRMHR)
      walking <- walking
      data <- walking$LEHHRMHR
      data <- data[data$LEHHRMHR > input$minrate,]
      data <- data[data$LEHHRMHR < input$maxrate,]
      ggplot(data = data) +
         geom_histogram(aes(x = LEHHRMHR, y = ..density..)) +
         geom_density(aes(x = LEHHRMHR), col = 2)
   })
   
   output$p2 <- renderPlot({
      input$dataset
      walking <- walking
      data <- walking$LEHHRMHR
      data <- data[data$LEHHRMHR > input$minrate,]
      data <- data[data$LEHHRMHR < input$maxrate,]
      ggplot(data = data) +
         geom_point(aes(x = Timestamp, y = LEHHRMHR, color = LEHHRMHR)) +
         geom_smooth(aes(x = Timestamp, y = LEHHRMHR),
                     method = "glm",
                     col = 2)
   })
   
   output$p3 <- renderPlot({
      input$dataset
      walking <- walking
      data <-
         merge(walking$ANDGPSLA,
               walking$ANDGPSLO,
               by = c("Date", "Timestamp"))
      data <-
         merge(data, walking$ANDGPSSP, by = c("Date", "Timestamp"))
      data <- data[data$ANDGPSSP > input$minspeed,]
      data <- data[data$ANDGPSSP < input$maxspeed,]
      ggplot(data = data) +
         geom_histogram(aes(x = ANDGPSSP, y = ..density..)) +
         geom_density(aes(x = ANDGPSSP), col = 2)
   })
   
   output$p4 <- renderPlot({
      input$dataset
      walking <- walking
      data <-
         merge(walking$ANDGPSLA,
               walking$ANDGPSLO,
               by = c("Date", "Timestamp"))
      data <-
         merge(data, walking$ANDGPSSP, by = c("Date", "Timestamp"))
      data <- data[data$ANDGPSSP > input$minspeed,]
      data <- data[data$ANDGPSSP < input$maxspeed,]
      ggplot(data = data) +
         geom_point(aes(x = Timestamp, y = ANDGPSSP, color = ANDGPSSP)) +
         geom_smooth(aes(x = Timestamp, y = ANDGPSSP),
                     method = "glm",
                     col = 2)
   })
   
   output$p5 <- renderLeaflet({
      input$dataset
      walking <- walking
      data <-
         merge(walking$ANDGPSLA,
               walking$ANDGPSLO,
               by = c("Date", "Timestamp"))
      data <-
         merge(data, walking$ANDGPSSP, by = c("Date", "Timestamp"))
      colors <- heat.colors(7)[floor(data$ANDGPSSP) + 1]
      colors <- substr(colors, 1, 7)
      colors[is.na(colors)] <- "#aaaaaa"
      leaflet() %>%
         addTiles() %>%
         addCircleMarkers(
            data = data.frame(lat = data$ANDGPSLA, lng = data$ANDGPSLO),
            radius = 2,
            weight = 0,
            fillColor = colors,
            fillOpacity = 0.5
         )
      
   })
   
}