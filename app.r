Longitude <- NULL # nolint
Latitude <- NULL # nolint

library(shiny)
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(lubridate)) {
  install.packages("lubridate")
}
# Define UI
ui <- fluidPage(
  titlePanel("CSV File Upload and Filter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )),
      textInput("filter_value", "Filter Value", ""),
      uiOutput("column_select_ui"), # Dynamic UI for column selection
      dateRangeInput("date_range", "Date Range",
                     start = Sys.Date() - 30, # Default start date
                     end = Sys.Date()), # Default end date
      actionButton("plot_btn", "Plot") # Button for plotting
    ),
    mainPanel(
      plotOutput("plot"), # Placeholder for the plot
      tableOutput("contents") # Display filtered contents
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read the uploaded file
  uploaded_file <- reactive({
    in_file <- input$file_1
    if (is.null(in_file)) {
      return(NULL)
    } else {
      tryCatch(
        read.csv(in_file$datapath),
        error = function(e) {
          showNotification(
            "Error reading file: Please ensure the file is a valid CSV.", type = "error" # nolint
          )
          return(NULL)
        }
      )
    }
  })

  #Dynamically generate UI for column selection based on uploaded file
  output$column_select_ui <- renderUI({
    df <- uploaded_file()
    if (is.null(df)) {
      return(NULL)
    } else {
      column_names <- names(df)
      selectInput(
        "selected_column", "Select Column to Filter", choices = column_names
      )
    }
  })
  # Update choices for selectInput based on uploaded file
  observe({
    df <- uploaded_file()
    updateSelectInput(session, "selected_column", choices = names(df))
  })

  # Filter the data based on selected column, filter value, and date range
  filtered_data <- reactive({
    df <- uploaded_file()
    selected_column <- input$selected_column
    filter_value <- input$filter_value
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    if (is.null(df) || is.null(selected_column) || filter_value == "") {
      return(df)
    } else {
      df <- df[df[[selected_column]] == filter_value, ]
      print(df$`Date.Time`)
      df$`Date.Time` <- as.Date(df$`Date.Time`, "%d/%m/%Y")
      print(df$`Date.Time`)
      print(as.Date(start_date, "%d/%m/%Y"))
      print(as.Date(end_date, "%d/%m/%Y"))
      df <- df[df$`Date.Time` >= as.Date(start_date, "%d/%m/%Y") & df$`Date.Time` <= as.Date(end_date, "%d/%m/%Y"), ] # nolint
      return(df)
    }
  })


  # Render the plot when the 'Plot' button is clicked
  observeEvent(input$plot_btn, {
    output$plot <- renderPlot({
      df <- filtered_data() # Get the filtered dataset
      if (is.null(df)) return()
      # Plotting the animal movements with Latitude on the X-axis and Longitude on the Y-axis # nolint
      ggplot(df, aes(x = Latitude, y = Longitude)) + # Adjusted axes
        geom_point() +
        labs(title = "Animal Movements", x = "Latitude", y = "Longitude") +
        theme_minimal()
    })
  })
  #Render the filtered table
  output$contents <- renderTable({
    filtered_data()
  })
}


# Run the app
shinyApp(ui = ui, server = server)