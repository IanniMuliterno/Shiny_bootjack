library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bootstrap and Jacknife tests"),
  
  # barra lateral  com slider para entrada no tamanho da amostra
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("dist", "Distribution type:",
                   list("Normal" = "norm",
                        "Gamma" = "gam",
                        "Log-normal" = "lnorm",
                        "Exponential" = "exp",
                        "Weibull" = "wei")),
      uiOutput("paramet"),
      
      #htmlOutput("selectUI"),
      
      br(),
      sliderInput("amostra",
                  "Size of the sample:",
                  min = 5,
                  max = 100,
                  value = 10)
      #br(),
      
      #sliderInput("rep",
      #            "Number of resamples:",
      #            min = 50,
      #            max = 300,
      #            value = 50)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        #tabPanel("Summary", dataTableOutput("dis")),
        tabPanel("Plot",
                 # fluidRow(...)
                 plotOutput("plot1"),
                 plotOutput("plot2")
        )
      )
    )
    
  )))
