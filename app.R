
library(PhIIdesign)
library(clinfun)
library(shiny)
library(DT)
library(shinyWidgets)
library(shinyjs)

# UI -------------------------------------------------------------------------------------

ui <- fluidPage(
  
  # Title
  titlePanel("Phase II Clinical Trail Design"),
  
  
  
  # Add layout
  sidebarLayout(
    
    # Input
    sidebarPanel(
      
      numericInput('Alpha', 
                   'Type I error, α', 
                   min = 0,
                   max = 0.5,
                   step = 0.01,
                   value = 0.05),
      
      numericInput('Power', 
                   'Power, 1 - β', 
                   min = 0.5, 
                   max = 1, 
                   step = 0.01,
                   value = 0.8),
      
      numericInput('p0', 
                   'Undesirable response probability, p0(Null hypothesis)', 
                   min = 0,
                   max = 1,
                   step = 0.01,
                   value = 0.05),
      
      numericInput('p1', 
                   'Desirable response probability, p1(Alternative hypothesis)', 
                   min = 0,
                   max = 1,
                   step = 0.01,
                   value = 0.15),
      
      numericInput('nmax', 
                   'Maximum total sample size, Nmax (Simon II stage design: default 100; can be at most 500)', 
                   min = 0,
                   max = 500,
                   step = 1,
                   value = 100)
    ),
    
    # Output 
    mainPanel(
      
      # Output: Tabset w/ results, documentation and example
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Results",
                           h3("Fleming’s Single-Stage Design"),
                           checkboxInput("checkbox", "Show All Output", F), 
                           dataTableOutput("table1"),
                           em(textOutput(outputId = "caption")),
                           
                           hr(),
                           
                           h3("Simon's Two-Stage Design"),
                           dataTableOutput("table2"),
                           
                           em("n1: sample size for the first stage of accrual.
                           n2 (n2 = n - n1): sample size for the second stage of accrual.
                           n (n = n1 + n2): is the total sample size after the completion of second stage.
                           r1: bound for stopping at stage 1 (concluding H0).
                           r: bound for concluding H0 at stage 2."),
                           
                           hr(),
                           
                           #(Simon's two stage test(Optimal)
                           h3("Interpretation Example"),
                           textOutput("text")),
                  
                  tabPanel("Reference", 
                           verbatimTextOutput("info"),
 
                           #Reference
                           h5("Reference"),
                           h6("Simon R (1989). Optimal two-stage designs for phase II clinical trials, Controlled Clinical Trials 10: 1-10."),
                           h6("Fleming TR (1982). One sample multiple testing procedure for phase II clinical trials, Biometrics 38: 143-151."),
                           
                           hr(),
                           
                           #Link
                           tags$a(href='www/phaseII.html', 
                                 target='blank', 
                                 'More Information'),
                           includeHTML("www/phaseII.html")
                          
                          )
                  
      )
    )
  )
)


# Server -----------------------------------------------------------------------------------
server <- function(input, output) {
  
  
  #Fleming output   
  observe({

    
  if(input$checkbox){
    
    
    flem1 <- reactive({  
      
      fleming <- fleming1stage(
        p0 = input$p0,
        pa = input$p1,
        alpha = input$Alpha,
        beta = 1 - input$Power
      ) 
      colnames(fleming)[2] <- "n"
      fleming[,c(3:2, 4:8)]
    })
    
    output$table1 <- renderDataTable({
      fun1 <- flem1()
      fun1
    }, 
    extensions = "ColReorder",
    options = list(
      initComplete = JS(
        "function(settings, json){",
        "$(this.api().table().header()).css({'background-color': '#87CEEB', 'color': '#000000'});",
        "}"),
      dom = 't',
      ordering = F), 
    rownames= F
    )
    
    #Caption of Fleming output 
    output$caption <- renderText({ "n: is the total sample size of participents.
      r: bound for concluding H0.
      eff: (r+1)/n (Reject H0 if number of patients who experience a response is larger than r+1).
      CI lower limit: exact 1-2*alpha confidence interval lower limit of eff.
      CI upper limit: exact 1-2*alpha confidence interval upper limit of eff.
      alpha: the actual alpha value which is smaller than alpha_param.
      beta: the actual beta value where which is smaller than beta_param." })
    
    
  }else{
    
    
    flem <- reactive({  
      
      fleming <- fleming1stage(
        p0 = input$p0,
        pa = input$p1,
        alpha = input$Alpha,
        beta = 1 - input$Power
      ) 
      fleming[, c(3:2)]
    })
    
    output$table1 <- renderDataTable({
      fun1 <- flem()
      fun1
    }, 
    colnames = c('r', 'n'),
    extensions = "ColReorder",
    options = list(
      initComplete = JS(
        "function(settings, json){",
        "$(this.api().table().header()).css({'background-color': '#87CEEB', 'color': '#000000'});",
        "}"),
      dom = 't',
      ordering = F), 
    rownames= F
    )
    
    #Caption of Fleming output 
    output$caption <- renderText({ "n: is the total sample size of participents. 
      r: bound for concluding H0. " })
    
    }
  })

    
  
#Simon output  
  output$table2 <- renderDataTable({
    
    fun2 <- ph2simon(pu = input$p0,
                     pa = input$p1,
                     ep1 = input$Alpha,
                     ep2 = 1 - input$Power,
                     nmax = input$nmax)
    
    tab <- rbind(fun2$out[which.min(fun2$out[, 5]), ], fun2$out[1, ])
    tab <- round(tab, 3)
    tab <- cbind(" " = c("Optimal", "Minimax"), tab)
    tab 
  }, 
  
  options = list(
    initComplete = JS(
      "function(settings, json){",
      "$(this.api().table().header()).css({'background-color': '#87CEEB', 'color': '#000000'});",
      "}"),
    dom = 't',
    ordering = F
   ), 
  rownames = F)
  
  output$text <- renderText({
    fun <- ph2simon(
      input$p0, 
      input$p1, 
      input$Alpha, 
      (1 - input$Power), 
      input$nmax)
    
    paste(
      "Using Simon's optimal two-stage design, we test the null hypothesis that the true response rate is", 
      input$p0,
      "against the alternative hypothesis that the true response rate is",
      input$p1,
      ". In the first stage,",
      fun$out[which.min(fun$out[, 5]), 2],
      "participants will be accrued. If there are",
      fun$out[which.min(fun$out[, 5]), 1],
      "or fewer responses from these",
      fun$out[which.min(fun$out[, 5]), 2],
      "participants, the trial will end. If not, go to the second stage, where",
      (fun$out[which.min(fun$out[, 5]), 4] - fun$out[which.min(fun$out[, 5]), 2]),
      "additional participants will be accrued for a total sample size of",
      fun$out[which.min(fun$out[, 5]), 4],
      ". We will reject the null hypothesis if we observe",
      (fun$out[which.min(fun$out[, 5]), 3] + 1),
      "or more responses from",
      fun$out[which.min(fun$out[, 5]), 4],
      "participants. The design yields a type I error rate at", 
      input$Alpha, 
      "and power of",
      input$Power, 
      ".") 
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
