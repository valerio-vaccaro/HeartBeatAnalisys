## ui.R ##
library(shinydashboard)
library(leaflet)

filenames <- list.dirs(path = "./Data", full.names = F)

ui <- dashboardPage(
   dashboardHeader(
      title = "My Activity Dashboard",
      dropdownMenu(
         type = "messages",
         messageItem(from = "Valerio Vaccaro",
                     message = "I hope you like my dashboard!!!"),
         messageItem(
            from = "Valerio Vaccaro",
            message = "Source code",
            icon = icon("question")
         )
      )
   ),
   
   ## Sidebar content
   dashboardSidebar(
      sidebarMenu(
         menuItem(
            "MyDashboard",
            tabName = "dashboard",
            icon = icon("dashboard")
         ),
         menuItem("Credits", tabName = "credits", icon = icon("th"))
      ),
      
      selectInput(inputId = "dataset", label = "Choose Dataset", filenames),
      fileInput(
         'newfile',
         'Import a new Dataset',
         accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      actionButton("updatebutton", "Update the dataset", width = '90%'),
      
      sliderInput(
         "minspeed",
         "Select Min speed",
         min = 0,
         max = 20,
         value = 0
      ),
      sliderInput(
         "maxspeed",
         "Select Max speed",
         min = 0,
         max = 20,
         value = 10
      ),
      sliderInput(
         "minrate",
         "Select Min heart rate",
         min = 0,
         max = 200,
         value = 60
      ),
      sliderInput(
         "maxrate",
         "Select Max heart rate",
         min = 0,
         max = 200,
         value = 200
      )
   ),
   dashboardBody(tabItems(
      # dashboard content
      tabItem(
         tabName = "dashboard",
         fluidRow(
            infoBoxOutput("dateBox"),
            infoBoxOutput("speedBox"),
            infoBoxOutput("pulseBox")
         ),
         fluidRow(
            box(
               title = "Map of the activity",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               leafletOutput("p5", height = 300)
            )
         ),
         fluidRow(
            box(
               title = "Heart rate during activity",
               status = "primary",
               solidHeader = TRUE,
               plotOutput("p2", height = 300)
            ),
            box(
               title = "Heart rate histogram",
               status = "primary",
               solidHeader = TRUE,
               plotOutput("p1", height = 300)
            )
         ),
         fluidRow(
            box(
               title = "Speed during activity",
               status = "primary",
               solidHeader = TRUE,
               plotOutput("p4", height = 300)
            ),
            box(
               title = "Speed histogram",
               status = "primary",
               solidHeader = TRUE,
               plotOutput("p3", height = 300)
            )
         )
         
      ),
      # credits content
      tabItem(
         tabName = "credits",
         h2("My Activity Dashboard"),
         "My Activity Dashboard is a project developed for the Coursera course called Developing Data Products and show how data can be managed and presentede using Shiny.",
         br(),
         "This dashoard shows the results on walking trip.",
         br(),
         "The data was recored using a Motorola Moto G Android phone with the SenseView application (",
         a(
            "https://play.google.com/store/apps/details?id=si.mobili.senseview"
         ),
         ") installed and connected to a Polar H7 belt (",
         a(
            "http://www.polar.com/us-en/products/accessories/H7_heart_rate_sensor"
         ),
         ") in order to record the heart rate data.",
         " Information from the GPS and acceletarion sensor are also saved and exported at the end of the trip.",
         br(),
         "This dashboard offer a view on all data recorded during a single trip.",
         br(),
         "The complete code is available on my github repository at ",
         a(
            "https://github.com/valerio-vaccaro/developing-data-products---MyActivityDashboard-assignement."
         ),
         br(),
         br(),
         "Valerio Vaccaro"
      )
   ))
)
