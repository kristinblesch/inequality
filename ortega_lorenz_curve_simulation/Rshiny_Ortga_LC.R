library(shiny)
library(ggplot2)
ui <- fluidPage(
  # App title 
  titlePanel("Ortega Lorenz curve model"),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
      
      # Slider for Ortega parameter 1
      sliderInput(inputId = "ortega_1", 
                  label = "Ortega Parameter 1 (alpha):",
                  min = 0, max = 10, step = 0.2,
                  value = 0.5),
      # Slider for Ortega parameter 2
      sliderInput(inputId = "ortega_2",
                  label = "Ortega Parameter 2 (beta):",
                  min = 0.001, max = 1,
                  value = 0.5),
      # Show Gini index associated
      textOutput(outputId = "gini")
    ),
  mainPanel(
      plotOutput(outputId = "lcplot")
  )
  )
)

server <- function(input, output){
  # Define Ortega Lorenz curve 
  ortega <- function(pop_csum, theta){ 
    # alpha = theta[1], beta = theta[2]
    # 0 <= alpha, 0 < beta <= 1
    pop_csum^theta[1] * (1- (1- pop_csum)^theta[2])
  }
  # Calculate associated Gini index:
  # Exaxt formula for calculating the Gini index for Ortega Lorenz curves, see Ortega et al. (1991)
  gini_ortega <- function(x){(x[1] - 1)/(x[1] +1) + 2*beta(x[1]+ 1, x[2]+ 1) }

  output$lcplot <- renderPlot({
    # define relevant values on x and y axis
    x_values <- sort(c(0,1,runif(1000, min = 0, max = 1)), decreasing = F)
    y <- ortega(pop_csum = x_values, theta = c(input$ortega_1, input$ortega_2))
    # actual plot
    ggplot()+
      geom_line(aes(x=x_values, 
                    y=ortega(x_values, theta = c(input$ortega_1, input$ortega_2))))+
      geom_line(aes(x=x_values, y=x_values), col = "grey")+
      geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
      theme(legend.position="bottom", legend.title = element_blank() )+
      labs(title="Resulting Lorenz curve",
           x="cumulative share of population", y = "cumulative share of income")+
      theme(plot.title = element_text(hjust = 0.5)) 
  }
  , height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0)))
  
  output$gini <- renderText({ 
    paste0("Gini index: ",round(gini_ortega(c(input$ortega_1, input$ortega_2)), digits = 4))
  })
}


library(rsconnect)
deployApp()
shinyApp(ui, server)


