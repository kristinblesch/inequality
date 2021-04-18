#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(colorspace)

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
                  label = "Ortega Parameter alpha:",
                  min = 0, max = 5, step = 0.01,
                  value = 0.5),
      # Slider for Ortega parameter 2
      sliderInput(inputId = "ortega_2",
                  label = "Ortega Parameter gamma:",
                  min = -1, max = -0.001,
                  value = -0.5),
      # Show US averages
      tags$hr(style="border-color: black;"),
      tags$hr(style="border-color: black;"),
      textOutput(outputId = "US_average"),
      # Show Gini index associated
      textOutput(outputId = "gini"),
      tags$hr(style="border-color: black;"),
      tags$hr(style="border-color: black;"),
      # add dropdown menu
      selectizeInput("county", "Get an empirical value from a US County (you can select multiple counties):", choices = NULL, selected=NULL, multiple = TRUE,options = list(create = TRUE, placeholder = 'type in a county name')),
      tableOutput("data_1"),
      
      selectizeInput("state", "Get an empirical value from a US State (you can select multiple states):", choices = NULL, selected=NULL, multiple = TRUE,options = list(create = TRUE, placeholder = 'type in a state name')),
      tableOutput("data_2"),
    ),
    # main panel - plot 
    mainPanel(
      plotOutput(outputId = "lcplot")
      
    )
  )
)

server <- function(input, output, session){
  # Define Ortega Lorenz curve 
  ortega <- function(pop_csum, theta){ 
    # alpha = theta[1], gamma = -theta[2]
    # 0 <= alpha, 0 < beta <= 1
    pop_csum^theta[1] * (1- (1- pop_csum)^(-theta[2]))
  }
  # Calculate associated Gini index:
  # Exaxt formula for calculating the Gini index for Ortega Lorenz curves, see Ortega et al. (1991)
  gini_ortega <- function(x){(x[1] - 1)/(x[1] +1) + 2*beta(x[1]+ 1, -x[2]+ 1) }
  
  
  
  output$gini <- renderText({ 
    paste0("Gini index using the above selected parameters: ",round(gini_ortega(c(input$ortega_1, input$ortega_2)), digits = 4))
  })
  df <- read.csv("ortega_parameters_gamma_county.csv")
  df_state <- read.csv("ortega_parameters_gamma_state.csv")
  ##asceding order
  sort_asc <- I("[{field: 'name', direction: 'asc'},{field: '$score'}]")
  opts <- list(create=TRUE,
               labelField =  'name',
               searchField = 'name',
               sortField = sort_asc, 
               placeholder = 'type in a county name'
  )
  
  updateSelectizeInput(session, "county", choices =df$COUNTY, selected=character(0),options = opts)
  output$data_1 <- renderTable({
    df[df$COUNTY %in% input$county, c("COUNTY", "alpha", "gamma", "Gini")]
  }, rownames = FALSE)
  updateSelectizeInput(session, "state", choices = as.character(df_state$STATE), selected=character(0),options = list(create = TRUE, placeholder = 'type in a state name'))
  output$data_2 <- renderTable({
    df_state[df_state$STATE %in% input$state, c("STATE", "alpha", "gamma", "Gini")]
  }, rownames = FALSE)
  output$US_average <- renderText({"The US average (calculated as population weighted county-level average) is \nalpha = 0.6125,\ngamma = -0.4156"})
  
  output$lcplot <- renderPlot({
    # define relevant values on x and y axis
    # --> calculate lorenz curve values for selected counties & states
    x_values <- sort(c(0,1,runif(1000, min = 0, max = 1)), decreasing = F)
    selected_parameters <- ortega(pop_csum = x_values, theta = c(input$ortega_1, input$ortega_2))
    df_subset <- df[match(input$county, df$COUNTY),]
    y_values <- apply(matrix(df_subset[,"COUNTY"], ncol = 1),1, 
                      function(x){ortega(x_values, c(df_subset[df_subset$COUNTY ==x, "alpha"],
                                                     df_subset[df_subset$COUNTY ==x, "gamma"]))})
    
    df_subset_2 <- df_state[match(input$state, df_state$STATE),]
    y_values_2 <- apply(matrix(df_subset_2[,"STATE"], ncol = 1),1, 
                        function(x){ortega(x_values, c(df_subset_2[df_subset_2$STATE ==x, "alpha"],
                                                       df_subset_2[df_subset_2$STATE ==x, "gamma"]))})
    
    # actual plot 
    # differentiate cases for which no counties and/or no states are selected
    
    if (nrow(df_subset) == 0 & nrow(df_subset_2) ==0){
      ggplot()+
        geom_line(aes(x=x_values, 
                      y=ortega(x_values, theta = c(input$ortega_1, input$ortega_2)), colour= "selected_parameters" ), size = 2)+ 
        geom_line(aes(x=x_values, y=x_values), col = "grey")+
        geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
        theme(legend.position="bottom", legend.title = element_blank() )+
        labs(title="Lorenz curve",
             x="cumulative share of population", y = "cumulative share of income")+
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values=c("black"))
    }else if (nrow(df_subset) != 0 & nrow(df_subset_2) ==0){
      colnames(y_values) <- df_subset[,"COUNTY"]
      dat <- cbind(selected_parameters, y_values, x_values) %>% as.data.frame() %>% 
        tidyr::gather(key = "id", value = "value", 1:(nrow(df_subset)+1)) 
      dat$id <- factor(dat$id, levels = unique(dat$id))
      ggplot() + 
        geom_line(data =dat, aes(x = x_values, y = value, col = id), size = 2) + 
        #     geom_line(aes(x=x_values, 
        #                  y=ortega(x_values, theta = c(input$ortega_1, input$ortega_2)),colour= factor("selected parameters") ), size = 2)+ 
        geom_line(aes(x=x_values, y=x_values), col = "grey")+
        geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
        theme(legend.position="bottom", legend.title = element_blank() )+
        labs(title="Lorenz curve",
             x="cumulative share of population", y = "cumulative share of income")+
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values=c("black", sequential_hcl(8, palette = "Plasma")[1:nrow(df_subset)]))
      
    } else if (nrow(df_subset) == 0 & nrow(df_subset_2) !=0){
      
      colnames(y_values_2) <- df_subset_2[,"STATE"]
      dat_2 <- cbind(selected_parameters, y_values_2, x_values) %>% as.data.frame() %>% 
        tidyr::gather(key = "id", value = "value", 1:(nrow(df_subset_2)+1)) 
      dat_2$id <- factor(dat_2$id, levels = unique(dat_2$id))
      ggplot() + 
        geom_line(data = dat_2, aes(x = x_values, y = value, col = id), size = 2) +
        geom_line(aes(x=x_values, y=x_values), col = "grey")+
        geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
        theme(legend.position="bottom", legend.title = element_blank() )+
        labs(title="Lorenz curve",
             x="cumulative share of population", y = "cumulative share of income")+
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values=c("black",sequential_hcl(8, palette = "Oranges")[1:nrow(df_subset_2)]))
    } else if (nrow(df_subset) != 0 & nrow(df_subset_2) !=0){
      colnames(y_values) <- df_subset[,"COUNTY"]
      dat <- cbind(selected_parameters, y_values, x_values) %>% as.data.frame() %>% 
        tidyr::gather(key = "id", value = "value", 1:(nrow(df_subset)+1)) 
      colnames(y_values_2) <- df_subset_2[,"STATE"]
      dat_2 <- cbind(y_values_2, x_values) %>% as.data.frame() %>% 
        tidyr::gather(key = "id", value = "value", 1:nrow(df_subset_2)) 
      dat_full <- rbind(dat,dat_2)
      dat_full$id <- factor(dat_full$id, levels = unique(dat_full$id))
      
      ggplot() + 
        geom_line(data = dat_full,aes(x = x_values, y = value, col = id), size =2)+
        geom_line(aes(x=x_values, y=x_values), col = "grey")+
        geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
        theme(legend.position="bottom", legend.title = element_blank() )+
        labs(title="Lorenz curve",
             x="cumulative share of population", y = "cumulative share of income")+
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual(values=c("black", sequential_hcl(8, palette = "Plasma")[1:(length(unique(dat$id))-1)],
                                    sequential_hcl(8, palette = "Oranges")[1:length(unique(dat_2$id))]))
    } else {print("error")}
    
  }
  , height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*3/5,0))
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
