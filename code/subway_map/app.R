library(shiny)
library(ggforce)
library(ggiraph)
library(tidyverse)

ui <- fluidPage(

    # Application title
    titlePanel(""),
        mainPanel(
           girafeOutput("gg_subway")
        )
)

server <- function(input, output) {

    output$gg_subway <- renderGirafe({
        subway <- tribble(
            ~station, ~x, ~y, ~onclick, ~type, 
            "Intro to Data Science (tutorial)", 0, 1, 'window.open("https://akshay-bareja.shinyapps.io/ds_intro_track/")', "tutorial",
            "Intro to Data Science (video)", 0, 0.99, 'window.open("https://youtu.be/TnPlET9mk0s")', "video",
            "Intro to Data Science (tutorial)", 0, 0.98, 'window.open("https://akshay-bareja.shinyapps.io/ds_intro_track/")', "tutorial",
            "Intro to Data Science (video)", 0, 0.97, 'window.open("https://youtu.be/TnPlET9mk0s")', "video",
            "Intro to Dplyr (tutorial)", 0.2, 1, 'window.open("https://bespokds.netlify.app/xaringan_intro_to_dplyr/#1")', "tutorial",
            "Intro to Dplyr (video)", 0.2, 0.99, 'window.open("https://youtu.be/W3D-JSPExXo")', "video",
            "Intro to Pandas (tutorial)", 0.2, 0.75, '', "tutorial",
            "Intro Pandas (video)", 0.2, 0.74, '', "video",
            "Selecting Variables Using Dplyr (tutorial)", 0.4, 1, 'window.open("https://bespokds.netlify.app/xaringan_select/#1")', "tutorial",
            "Selecting Variables Using Dplyr (video)", 0.4, 0.99, 'window.open("https://youtu.be/vDgP7J1qMCI")', "video",
            "Selecting Variables Using Pandas (tutorial)", 0.4, 0.75, '', "tutorial",
            "Selecting Variables Using Pandas (video)", 0.4, 0.74, '', "video"
        )
        
        gg_subway <- ggplot(subway, aes(x = x, y = y)) +
            # tracks
            geom_segment(x = 0, xend = 0.2, y = 1, yend = 1, size = 5, colour = "blue") +
            geom_segment(x = 0, xend = 0.2, y = 0.99, yend = 0.99, size = 5, colour = "red") +
            geom_curve(x = 0, xend = 0.2, y = 0.98, yend = 0.75, size = 5, colour = "orange",
                       curvature = 0.05) +
            geom_curve(x = 0, xend = 0.2, y = 0.97, yend = 0.74, size = 5, colour = "darkgreen",
                       curvature = 0.05) +
            geom_segment(x = 0.2, xend = 0.2, y = 0.99, yend = 0.75, linetype = 2) +
            geom_segment(x = 0.2, xend = 0.4, y = 1, yend = 1, size = 5, colour = "blue") +
            geom_segment(x = 0.2, xend = 0.4, y = 0.99, yend = 0.99, size = 5, colour = "red") +
            geom_segment(x = 0.2, xend = 0.4, y = 0.75, yend = 0.75, size = 5, colour = "orange") +
            geom_segment(x = 0.2, xend = 0.4, y = 0.74, yend = 0.74, size = 5, colour = "darkgreen") +
            geom_segment(x = 0.4, xend = 0.4, y = 0.99, yend = 0.75, linetype = 2) +
            # stations
            geom_point(colour = "black", size = 2.5, stroke = 2) +
            geom_point_interactive(aes(tooltip = station, onclick = onclick), size = 1.9, colour = "white") +
            # track legend
            annotate("segment",
                     x = 0.05, xend = 0.1,
                     y = 1.06, yend = 1.06,
                     size = 1,
                     colour = "blue") +
            annotate("segment",
                     x = 0.05, xend = 0.1,
                     y = 1.04, yend = 1.04,
                     size = 1,
                     colour = "red") +
            annotate("text", 
                     label = "R track (tutorial)",
                     x = 0.135, y = 1.06,
                     size = 3,
                     fontface = "bold") +
            annotate("text",
                     label = "R track (video)",
                     x = 0.131, y = 1.04,
                     size = 3,
                     fontface = "bold") +
            annotate("segment",
                     x = 0.25, xend = 0.3,
                     y = 1.06, yend = 1.06,
                     size = 1,
                     colour = "orange") +
            annotate("segment",
                     x = 0.25, xend = 0.3,
                     y = 1.04, yend = 1.04,
                     size = 1,
                     colour = "darkgreen") +
            annotate("text", 
                     label = "Python track (tutorial)",
                     x = 0.347, y = 1.06,
                     size = 3,
                     fontface = "bold") +
            annotate("text",
                     label = "Python track (video)",
                     x = 0.345, y = 1.04,
                     size = 3,
                     fontface = "bold") +
            # station names
            annotate("text",
                     label = "Intro",
                     x = 0, y = 1.025,
                     size = 3,
                     angle = 35,
                     fontface = "bold") +
            annotate("text",
                     label = "Dplyr",
                     x = 0.2, y = 1.025,
                     size = 3,
                     angle = 35,
                     fontface = "bold") +
            annotate("text",
                     label = "Pandas",
                     x = 0.2, y = 0.71,
                     size = 3,
                     angle = 35,
                     fontface = "bold") +
            annotate("text",
                     label = "Select",
                     x = 0.4, y = 1.025,
                     size = 3,
                     angle = 35,
                     fontface = "bold") +
            annotate("text",
                     label = "Select",
                     x = 0.4, y = 0.71,
                     size = 3,
                     angle = 35,
                     fontface = "bold") +
            theme_void()
        
        girafe(ggobj = gg_subway)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
