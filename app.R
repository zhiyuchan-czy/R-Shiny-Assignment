#install.packages("shiny")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("rsconnect")
#install.packages("devtools")

library(shiny)
library(readr)
library(ggplot2)
library(reshape2)

#devtools::install_github("rstudio/rsconnect")
rsconnect::writeManifest()

reactiveConsole(TRUE)

ui <- fluidPage(

  titlePanel("R Shiny Assignment"),

  sidebarLayout(

    sidebarPanel(
      fileInput(
        inputId = "file",
        label   = "Choose claims data file",
        accept  = c(".csv", ".xlsx")
      ),

      sliderInput(
        inputId = "tailfactor",
        label = "Tail Factor",
        value = 1.1,
        min = 0,
        max = 5,
        step = 0.1
      ),

      actionButton("process", "Click here")

    ),

    mainPanel(
      "Stimulating Cumulative Paid Claims",

      tableOutput("calc"),
      plotOutput("graph")

    )

  )

)



server <- function(input, output, session) {

  # Input data
  df <- reactive({
    req(input$file)

    read.csv(input$file$datapath,
             header = TRUE,
             sep = ",")

  })

  # Raw data table
  output$table <- renderTable(df())

  # Calculate claims table
  calc_claims = eventReactive(input$process, {

    #Calculation
    data = df()
    data$Amount.of.Claims.Paid = as.numeric(gsub(",", "", data$Amount.of.Claims.Paid))

    claim_matrix = tapply(data$Amount.of.Claims.Paid, list(data$Loss.Year, data$Development.Year), sum)
    claim_matrix[is.na(claim_matrix)] = 0

    cum_matrix = matrix(0, nrow = nrow(claim_matrix), ncol = ncol(claim_matrix) + 1)
    for (i in 1:nrow(claim_matrix)){
      for (j in 1:ncol(claim_matrix)){
        cum_matrix[i,j] = sum(claim_matrix[i,1:j])
      }
    }

    n = nrow(cum_matrix)
    f1 = sum(cum_matrix[1:(n-1),2], na.rm = TRUE) / sum(cum_matrix[1:(n-1),1], na.rm = TRUE)
    f2 = sum(cum_matrix[1:(n-2),3], na.rm = TRUE) / sum(cum_matrix[1:(n-2),2], na.rm = TRUE)

    cum_matrix[2,3] = cum_matrix[2,2] * f2
    cum_matrix[3,2] = cum_matrix[3,1] * f1
    cum_matrix[3,3] = cum_matrix[3,2] * f2

    for (k in 1:n) {
      cum_matrix[k,4] = cum_matrix[k,3] * input$tailfactor
    }

    cum_matrix

  })

  # Obtain table of claims
  output$calc = renderTable({

    # Format the final table
    final_matrix = format(round(data.frame(calc_claims()), 0), big.mark = ",", scientific = FALSE)
    colnames(final_matrix) = c("Development Year 1", "Development Year 2", "Development Year 3", "Development Year 4")
    rownames(final_matrix) = c("Loss Year 2017", "Loss Year 2018", "Loss Year 2019")
    final_matrix

  }, rownames = TRUE)

  # Plot graph
  output$graph = renderPlot({

    data <- calc_claims()
    colnames(data) = paste0("Development Year ", 1:4)
    rownames(data) = paste0("Loss Year ", 2017:2019)

    # Prepare data for graph
    data_ = melt(data, varnames = c("LossYear", "DevelopmentYear"), value.name = "Value")

    # Plot
    ggplot(data_, aes(x = DevelopmentYear, y = Value, colour = LossYear, group = LossYear)) +
      geom_point(size = 3) +
      geom_line(size = 2) +
      labs(title = "Cumulative Paid Claims ($)",
           x = "Development Year",
           color = "Loss Year") +
      theme_minimal() +
      theme(legend.position = "bottom")

  })
}

shinyApp(ui, server)
