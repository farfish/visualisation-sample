# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel(
    fluidRow(
      column(1, img(
        width = 89, height = 83,
        src = "https://www.farfish.eu/wp-content/uploads/2017/08/FarFish-300x281.png"
      )),
      column(9, "Farfish FFDB Catch Data")
    )
  , windowTitle = "Farfish FFDB Catch Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput(
          "document_name",
          "Document name",
          list())
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "catchPlot")

    )
  )
)
