# 240, 130, 0
# runApp("PotentialNavigator")
library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)


branchen = c("Tankstellen", "Spielwareneinzelhandel", "Sporteinzelhandel", "SanitÃ¤tsfachhandel",
             "Schuheinzelhandel", "Facheinzelhandel mit Nahrungs- und Genussmitteln", "Sortimentseinzel-
             handel mit Nahrungs- und Genussmitteln")

n = 300
set.seed(1)
dataRaw <- data.frame(
  name = paste0("Unternehmen", 1:n),
  lat = 51.343479 + rnorm(n, 0, 0.03),
  long = 12.387772 + rnorm(n, 0, 0.03),
  umsatz = sample(0:50000, n, replace = TRUE),
  entfernung = sample(0:100, n, replace = TRUE),
  bonitaet = sample(1:3, n, replace = TRUE),
  potential = sample(0:400, n, replace = TRUE),
  neuKunde = sample(c(T, F), n, replace = TRUE),
  branche = sample(branchen, n, replace = TRUE),
  gruendungsdatum = sample(1900:2005, n, replace = TRUE),
  stringsAsFactors = FALSE
)

dataRaw2 <- read.csv2("daten5.csv", sep = ";", dec = ",",
                     encoding = "UTF-8", stringsAsFactors = FALSE, header = TRUE)
dataRaw$name <- dataRaw2$Column2
dataRaw$lat <- dataRaw2$lat
dataRaw$long <- dataRaw2$lng
dataRaw2$Umsatz.normiert <- gsub("[.]", "", dataRaw2$Umsatz.normiert)
dataRaw2$Umsatz.normiert <- gsub(" ", "", dataRaw2$Umsatz.normiert)
dataRaw$umsatz <- as.numeric(as.numeric(dataRaw2$Umsatz.normiert) / 1000000)
dataRaw2$Ertragspotenzial <- gsub("[.]", "", dataRaw2$Ertragspotenzial)
dataRaw2$Ertragspotenzial <- gsub(" ", "", dataRaw2$Ertragspotenzial)
dataRaw$potential <- as.numeric(dataRaw2$Ertragspotenzial)
dataRaw$umsatz <- as.numeric(as.numeric(dataRaw2$Umsatz.normiert) / 1000000)
dataRaw$branche <- dataRaw2$WZ
dataRaw$neuKunde[201] <- TRUE
dataRaw$long[177] <- dataRaw$long[178]
dataRaw$lat[177] <- dataRaw$lat[178]
dataRaw$long[202] <- dataRaw$long[203]
dataRaw$lat[202] <- dataRaw$lat[203]
dataRaw$str <- dataRaw2$Straße
dataRaw$str[280] <- "Horbeller Straße 17"
dataRaw$plz <- dataRaw2$Column3
dataRaw$stadt <- dataRaw2$Column4
dataRaw$website <- dataRaw2$Web.URL
dataRaw$bonitaet[201] <- 1
dataRaw$ansprechpartner <- dataRaw2$Ansprechpartner

# idx <- which(dataRaw$branche == "Industrie/ Handwerk")
# dataRaw[idx, ]$umsatz[dataRaw[idx, ]$umsatz >100] <- 12

getColor <- function(data) {
  sapply(dataRaw$neuKunde, function(neuKunde) {
    if(neuKunde) {
      "orange"
    } else {
      "blue"
    } 
  })
}

icons <- awesomeIcons(
  markerColor = getColor(data)
)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$uiSearch <- renderUI({
    if(input$doNameSearch){
      return(
        column(2,
             selectizeInput("name",
                            "Unternehmensname:",
                            dataRaw$name, multiple = TRUE)
        )
      )
    }else{return(
      tagList(
        column(2, 
               sliderInput("umsatz",
                           "Umsatzerlöse:",
                           min = 5,
                           max = 15,
                           value = c(5, 15))
        ),
        column(2, 
               sliderInput("entfernung",
                           "Entfernung:",
                           min = min(dataRaw$entfernung),
                           max = max(dataRaw$entfernung),
                           value = c(min(dataRaw$entfernung), as.integer(0.6*max(dataRaw$entfernung))))
        ),
        column(2, 
               sliderInput("bonitaet",
                           "Bonität:",
                           min = min(dataRaw$bonitaet),
                           max = max(dataRaw$bonitaet),
                           value = c(min(dataRaw$bonitaet), max(dataRaw$bonitaet)),
                           step = 1)
        ),
        column(2,
               sliderInput("potential",
                           "Potential:",
                           min = 100,
                           max = 170,
                           value = c(110, 150))
        ),
        column(2,
               sliderInput("grDatum",
                           "Gründungsdatum:",
                           min = min(dataRaw$gruendungsdatum),
                           max = max(dataRaw$gruendungsdatum),
                           value = c(min(dataRaw$gruendungsdatum), max(dataRaw$gruendungsdatum)))
        ),
        column(2,
          selectizeInput("branche", "Branche:", choices = unique(dataRaw$branche), multiple = TRUE)
        )
      )
    )
      
    }
  })
  
  output$table <- DT::renderDataTable({
    datatable(data()[c("name", "potential", "neuKunde", "branche")], options = list(pageLength = 25))
  })
   
  data <- reactive({
    # to do: probably needs refactoring
    if(!input$doNameSearch){
      if(!is.null(input$umsatz) & !is.null(input$potential) & !is.null(input$entfernung)){
        if(!length(input$branche)){
          FilterBranche = rep(TRUE, n)
        }else{
          FilterBranche = dataRaw$branche %in% input$branche 
        }
        FilterUmsatz <- dataRaw$umsatz >= input$umsatz[1] & dataRaw$umsatz <= input$umsatz[2]
        print(FilterUmsatz)
        FilterEntfernung <- dataRaw$entfernung >= input$entfernung[1] & dataRaw$entfernung <= input$entfernung[2]
        FilterBonitaet <- dataRaw$bonitaet >= input$bonitaet[1] & dataRaw$bonitaet <= input$bonitaet[2]
        FilterPotential <- dataRaw$potential >= input$potential[1] & dataRaw$potential <= input$potential[2]
        FilterDatum <- dataRaw$gruendungsdatum >= input$grDatum[1] & dataRaw$gruendungsdatum <= input$grDatum[2]
        Filter <- FilterUmsatz & FilterEntfernung & FilterBonitaet & FilterPotential & FilterDatum & FilterBranche
      }else{
        Filter = rep(TRUE, n)
      }
    }else{
      if(!length(input$name)){
        Filter = rep(TRUE, n)
      }else{
        Filter <- dataRaw$name %in% input$name
      }
    }
    dataRaw[Filter, ]
  })
  
  output$map <- renderLeaflet({
    data <- data()
    
    
    popupHtml <- '<table class="popup-table">
    <tr>
    <td></td><td></td><td align="right" style="max-width: 40%%">
    <label for="id-of-input" class="custom-checkbox">
    <input type="checkbox" id="id-of-input"/>
    <i class="glyphicon glyphicon-star-empty"></i>
    <i class="glyphicon glyphicon-star"></i>
    </label>
    </td>
    </tr>
    <tr>
    <td width="60%%" valign="top">
    <h1>%s</h1>
    <div style="font-size: 16px">%s</div>
    <br/>
    <b>Ansprechpartner:</b> %s
    <br/>
    <b>Website:</b> <a href="%s">%s</a>
    <br/>
    <b>Umsatz:</b> %s TEUR
    <br/>
    <b>Bonität:</b> %s
    <br/>
    <br/>
    <button>CreFo anfordern</button>
    </td>
    <td width="2%%"></td>
    <td align="left" valign="top">
    <b>Ertragspotential:</b>
    <br/>
    <br/>
    <img src="Diagramm.png"></img>
    </td>'
    
    filledPopupHtml <- sprintf(popupHtml, data$name, paste(paste0(data$str, ","), data$plz, data$stadt), data$ansprechpartner, data$website, data$website, data$umsatz, paste0(data$bonitaet, "B"), data$name);
   
    map <- leaflet() %>%
      addTiles() %>%
      addAwesomeMarkers(lng = data$long, lat= data$lat, icon =icons, popup=filledPopupHtml, popupOptions = popupOptions(
        closeButton = FALSE,
        minWidth = 500,
        maxWidth = 500
      ))
    map <- addControlGPS(map, options = gpsOptions(position = "topleft", activate = TRUE,
                                                   autoCenter = TRUE, maxZoom = 10,
                                                   setView = TRUE))
    activateGPS(map)
    return(map)
  })
})


