library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(tools)

package_docs <- function(pkg) {
    help_dir <- system.file("help", package = pkg)
    db_path <- file.path(help_dir, pkg)
    tools:::fetchRdDB(db_path)
}
# this function was taken from - https://community.rstudio.com/t/programmatically-accessing-param-fields-values-of-functions-from-installed-package/14731

verbs <- c("determine", "modify", "construct", "define", "control", "create", "convert", "coerce", "benchmark", "calculate",
           "take", "map", "discretise", "generate", "render", "expand", "facet", "wrap", "find", "fortify", "supplement",
           "draw", "count", "connect", "extract", "add", "build", "save", "visualise", "get", "retrieve", "label", 
           "nudge", "compute", "use", "scale", "specify", "summarise", "transform", "tidy", "quote", "arrange", "update")
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Tidybiology Quiz"),
    
    fluidRow(
        column(3,
               textInput("package", "Package Name: ")
               ),
        
        column(3,
               tags$head(
                   tags$style(HTML('#question{background-color:blue;
                                color:white}'))
               ),
               actionButton("question", "Next Question", width = '150px', icon = icon('refresh')),
               tags$head(
                   tags$style(HTML('#answer{background-color:blue;
                                    color:white}'))
               ),
               uiOutput("button2")
        ),
        
        column(9,
               tags$head(
                   tags$style(HTML('#function_question{font-size: 25px;
                                              text-align: center}'))
               ),
               textOutput("function_question"),
               tags$head(
                   tags$style(HTML('#function_name{font-size: 25px;
                                         text-align: center;
                                         color:teal}'))
               ),
               textOutput("function_name"),
               tags$head(
                   tags$style(HTML('#function_url{font-size: 20px;
                                         text-align: center;
                                         color:gray}'))
               ),
               uiOutput("function_url")
        )
    )
)

server <- function(input, output) {
    
    rdb <- NULL
    index <- NULL
    
    observeEvent(input$question, {
    output$function_question <- renderText({})
    output$function_name <- renderText({})
    output$function_url <- renderUI({})
    
    rdb <<- reactive({
        package_docs(input$package)
    })    
    
    index <<- reactive({
        sample(length(rdb()), 1)
    })
    
    output$function_question <- reactive({
         start_word <- word(rdb()[[index()]][[1]][[1]][1])
         start_word <- tolower(start_word)
         
         if(start_word %in% verbs){
             paste0("How would you ", tolower(rdb()[[index()]][[1]][[1]][1]))
         } else{
             rdb()[[index()]][[1]][[1]][1]
         }
    })
    
    output$button2 <- renderUI({
        actionButton("answer", label = "Show Answer", width = '150px', icon = icon('check'))
    })
    })
    
    observeEvent(input$answer, {
        output$function_name <- renderText({})
        output$function_url <- renderUI({})
        
        fn_name <- rdb()[[index()]][[2]][[1]][1]
        output$function_name <- renderText({fn_name})
        
        # from - https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
        url <- a("Link", 
                 href= paste0("https://www.rdocumentation.org/packages/",
                              input$package,
                              "/versions/",
                              packageVersion(input$package),
                              "/topics/",
                              fn_name
                              ))
        output$function_url <- renderUI({
            tagList("Function Docs:", url)
        })
        
        shinyjs::hide("answer")
    })
}

shinyApp(ui = ui, server = server)



