# project: fo_visual_library
# title: "FO Visual Library - R Shiny App"
# date: "2023-07-28"
# author: "Prasann Ranade"
# categories: [viz, tools, R]

#IMPORT-------------------------------------------------------------------------

# RENAME THIS SCRIPT TO "app.R" BEFORE RUNNING APP

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(DT)
library(reactable)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(gagglr)
library(glamr)
library(splitstackshape)

# RENAME THIS SCRIPT TO "app.R" BEFORE RUNNING APP

# authenticate secrets and access google sheets
load_secrets()
sheet_id <- '1qazek3Widi9kPbWnb684DjuFMFaK_nJiRthShuNKTMg'

# import from form response sheet
data <- read_sheet(as_sheets_id(sheet_id), "Form_Responses")
data <- cSplit(data, c("indicators", "source", "plot_type"), ",", direction="long", type.convert = "as.character")

#UI-----------------------------------------------------------------------------

# Define UI
ui <- navbarPage(
  "SI Visual Library",
  
  # include an image of the R Shiny app's workflow and links to the submission form and metadata table
  tabPanel(
    "Introduction",
    fluidRow(column(
      12,
      h2("Welcome to the SI Visual Library!"),
      p("This R Shiny app allows you to explore a dataset of visuals produced by the SI team."),
      p("Use the navigation bar above to access different features, 
        like documentation for this app, a searchable dataset, and a scrollable library.")
    )),
    imageOutput("photo"),
    tags$blockquote("This Visual Library is still under continuous development. 
           Please look forward to future updates!"),
    wellPanel(helpText(
      a("Click here to add your visuals to the library",
        href = "https://forms.gle/YBhN9KvMYxBYVthT6",
        target = "_blank")
    )),
    wellPanel(helpText(
      a("Click here to see a Google Sheets view of existing visuals",
        href = "https://docs.google.com/spreadsheets/d/1qazek3Widi9kPbWnb684DjuFMFaK_nJiRthShuNKTMg/edit?usp=sharing",
        target = "_blank")
    ))
  ),
  
  # display metadata table with searchable categories on the left
  tabPanel("Search",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(
                   "Indicator1",
                   "Select Indicator:",
                   choices = sort(unique(data$indicators)),
                   multiple = TRUE
                 ),
                 selectizeInput(
                   "Source1",
                   "Select Source:",
                   choices = sort(unique(data$source)),
                   multiple = TRUE
                 ),
                 selectizeInput(
                   "Plot_type1",
                   "Select a Plot Type:",
                   choices = sort(unique(data$plot_type)),
                   multiple = TRUE
                 )
               ),
               mainPanel(dataTableOutput("table"))
             )
  )),
  
  # create a tab for searching by images by clicking on example visuals
  tabPanel("Search by Images",
           fluidPage(  
             useShinyjs(), 
             img(id="img012",src="image012.png",width="19.5%",style="cursor:pointer;"),
             img(id="img003",src="image003.png",width="19.5%",style="cursor:pointer;"),
             img(id="img005",src="image005.png",width="19.5%",style="cursor:pointer;"),
             img(id="img011",src="image011.png",width="19.5%",style="cursor:pointer;"),
             img(id="img015",src="image015.png",width="19.5%",style="cursor:pointer;"),
             DT::dataTableOutput("dynamic")
           )
  ),
  
  # additional resources tab to connect to the intro to viz R scripts and the style guide
  tabPanel(
    "Resources",
    fluidRow(column(
      12,
      h2("Resources for building your own plots"),
      
      h4("- Intro to Viz scripts for those new to writing scripts in R.", a("Click here", href = "https://docs.google.com/document/d/1ANsqUwXLsJVfz_3kCG7iFn5FBR4Y8PPi/edit")),
      imageOutput("intro"),
      
      h4("- SI Style Guide for those looking to take their visuals to the next level.", a("Click here", href = "https://usaid-oha-si.github.io/styleguide/")),
      imageOutput("guide")
    )))
)


#SERVER-------------------------------------------------------------------------

# Define server
server <- function(input, output) {
  
  # this render image code creates the clickable visuals in the Search by Plot Type tab
  output$photo <- renderImage({
    list(
      src = "www/viz-library.png",
      contentType = "image/png",
      width = 500,
      height = 400
    )
  }, deleteFile = FALSE)
  
  output$guide <- renderImage({
    list(
      src = "www/style-guide.png",
      contentType = "image/png",
      width = 500,
      height = 400
    )
  }, deleteFile = FALSE)
  
  output$intro <- renderImage({
    list(
      src = "www/intro.png",
      contentType = "image/png",
      width = 600,
      height = 400
    )
  }, deleteFile = FALSE)
  
  # make a reactive data table with the selectizeinput functions created above
  filteredData <- reactive({
    data
    if (!is.null(input$Indicator1) || !is.null(input$Source1) || !is.null(input$Plot_type1)) {
      data %>%
        filter(if (!is.null(input$Indicator1)) { indicators == input$Indicator1 } else { TRUE }) %>%
        filter(if (!is.null(input$Source1)) { source == input$Source1 } else { TRUE }) %>%
        filter(if (!is.null(input$Plot_type1)) { plot_type == input$Plot_type1 } else { TRUE })
    } else {
      data
    }
  })
  
  # render the filtered table
  output$table <- renderDataTable(
    filteredData(), selection = list(mode = "single", target = "row")
  )
  
  # have a clickable metadata table that shows a link to the script (if a reference ID is provided)
  observeEvent(input$table_rows_selected, {
    req(input$table_rows_selected)
    showModal(modalDialog(
      title = "Link to script",
      paste0("https://github.com/search?q=org%3AUSAID-OHA-SI+", data[input$table_rows_selected,"link_to_script"], "&type=code"), 
      easyClose = TRUE,
      footer = NULL))
  })
  
  # plot types associated with the clickable images instantiated above
  selected_plot_type <- reactiveVal()
  shinyjs::onclick("img012",  selected_plot_type('Line Chart'))
  shinyjs::onclick("img003",  selected_plot_type('Targets vs. Results'))
  shinyjs::onclick("img005",  selected_plot_type('Heatmap Table'))
  shinyjs::onclick("img011",  selected_plot_type('World Map'))
  shinyjs::onclick("img015",  selected_plot_type('Linear scatterplot'))
  output$dynamic<- DT::renderDataTable({  
    req(selected_plot_type())
    data[data$plot_type == selected_plot_type(),]
  })
}

#APP----------------------------------------------------------------------------

# Run the app
shinyApp(ui, server)
