# -------------------------------------------
# Shiny Web Application Created by Michael Wu
# Gov 1005 Final Project
# -------------------------------------------

# Loading the necessary libraries

library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)

# ------------------------------------------------------------
# Loading the RDS data objects that were saved in my rmd files
# This is proper etiquette, DO NOT just copy your R code over
# ------------------------------------------------------------

new_york_unemployment <- readRDS("rdsObjects/new_york_unemployment.rds")
new_york_covidata <- readRDS("rdsObjects/new_york_covidata.rds")
# national_unemployment <- readRDS("rdsObjects/national_unemployment.rds")
# covidata_us_total <- readRDS("rdsObjects/covidata_us_total.rds")
joined_data <- readRDS("rdsObjects/joined_data.rds")
joined_data2 <- readRDS("rdsObjects/joined_data2.rds")

# -------------------------------------------------------------------------
# Front-End UI for the Shiny App, this is the aesthetic skeleton of the app
# Nothing too crazy here, just a few tabs with text and plots on them
# -------------------------------------------------------------------------

ui <- navbarPage(
        title = "the Coronavirus Project",
        theme = shinytheme("sandstone"),
        
        tabPanel(
            "Home",
             fluidPage(
                 br(),
                 imageOutput("banner_image", width = "100%", height = "100%"),
                 br(),
                 fluidRow(
                     column(2), column(8,
                        h2("Background", style = "color: darkred"),
                        h4("Insert some text here"),
                        
                        h2("Hypothesis", style = "color: darkred"),
                        h4("Insert some text here"),
                        
                        h2("Findings", style = "color: darkred"),
                        h4("Insert some text here"),
                        
                        br(), br(), br()
                    )
                )
        )),
                          
        # Second tab
           
        tabPanel(
            "National",
            fluidPage(
                    h4("National Unemployment Claims versus Coronavirus Cases"),
                    h4(" "), 
                    plotOutput("plot_scatter"), 
                    h4("National Unemployment Claims versus Coronavirus Growth"), 
                    h4(" "),
                    plotOutput("plot_scatter_deriv")
        )),
        
        # Third tab
        
        tabPanel("NY State",
                 fluidPage(
                    h4("New York State Unemployment Claims over Time"),
                    h4(" "), 
                    plotOutput("plot_ny1"), 
                    h4("New York State Coronavirus Growth Rate over Time:"), 
                    h4(" "),
                    plotOutput("plot_ny2")
        )),
        
        # Last 'About' tab, contains informaiton about data sources and the author
        
        tabPanel("About", 
                 fluidPage(
                     # Adding some basic CSS styling to the page
                     
                    tags$head(tags$style(HTML("a {color: #0000CD}"))),
                    tags$head(tags$style(HTML("h4 {line-height: 1.6; padding-top: 5px; text-align: justify; padding-left: 20px; padding-right: 20px;}"))),
                    tags$head(tags$style(HTML("h2 {padding-left: 20px; padding-right: 20px; padding-top: 20px; padding-bottom: 0px;}"))),
                    
                    fluidRow(
                        column(1), column(10,
                              h2("Project Description"),
                              h4("Created for my Gov 1005: Data Science class as our capstone final project.",
                                 ),
                              h2("Acknowledgements"),
                              h4("A huge thank you to the", a("Johns Hopkins University Coronavirus Database",
                                 href = "https://github.com/CSSEGISandData/COVID-19"), "for the incredible real-time data
                                 that they provide on their university GitHub page for the general public’s use. This work
                                 would also have not been possible without the open-source", a("US Department of Labor’s
                                 Unemployment Database", href = "https://oui.doleta.gov/unemploy/DataDownloads.asp"), ", 
                                 updated every week dating back to 1986 for all fifty states."),
                              h4("I’d also like to give a final thanks to my incredible Teaching Fellow Kaneesha Johnson
                                 for helping guide this project throughout its creation and also to the one and only
                                 David Kane, Preceptor of Gov 1005 for doing an incredible job acting as the guiding 
                                 rope to my Ulysses."),
                              h2("About the Creator"),
                              h4("My name is Michael Wu and I created this as a second-year student at Harvard College as a member of the class of 2022.
                                 Currently, I am pursuing a B.A. degree in Computer Science. I've built a number of websites in the past but this is 
                                 my first experience working with Shiny as the web hosting framework and R as the back-end.
                                 I grew up in sunny Orange County, California and enjoy
                                 playing basketball, poker, the TV show Suits, running, and doing yoga."),
                              h4("If you're interested in learning more about the project or about myself, don't hesistate
                                 to reach out through email at mdwu@college.harvard.edu, connect with me on",
                                 a("LinkedIn", href = "https://www.linkedin.com/in/michael-d-wu-809522145/"), 
                                 ", or visit my", a("GitHub Account", href="https://github.com/michaeldwu"), " page.
                                 Thanks for visiting!"),
                                          
                              br(), br(), br()
                        )
                    )
                    
                    )))

# --------------------------------------------------------------
# Back-end Server Logic for Shiny Apps
# This is where the "plot_output()" functions of the UI tab call
# Creates ggplots with the data objects stored in the RDS files
# --------------------------------------------------------------

server <- function(input, output, session) {
    
    # Coronavirus Banner Image
    
    output$banner_image <- renderImage({
        list(src = './images/corona-banner.png', 
             width = "80%",
             style = "display: block; margin-left: auto; margin-right: auto;")}, 
        deleteFile = FALSE)

    # Scatter Plots
    
    output$plot_ny1<-renderPlot({
        ggplot(new_york_unemployment, aes(x = date, y = initial_claims)) +
            geom_line() +
            geom_smooth(se = FALSE, span = 0.5) +
            labs(title = "Weekly Unemployment Filings in New York State",
                 subtitle = "As of April 24th",
                 x = "Date",
                 y = "Case Count") +
            theme_classic()
        },
        height = 400,
        width = 600
    )
    
    output$plot_ny2<-renderPlot({
        ggplot(new_york_covidata, aes(x = date, y = deriv_cases)) +
            geom_line() +
            geom_smooth(se = FALSE, span = 0.3) +
            labs(title = "Growth of Covid-19 Cases in New York State",
                 subtitle = "As of April 24th",
                 x = "Date",
                 y = "First Derivative of Case Count") +
            theme_classic()
        },
        height = 400,
        width = 600
    )
    
    output$plot_scatter<-renderPlot({
        ggplot(joined_data, aes(x = log_total_cases, y = log_initial_claims, color = date)) +
            geom_point() +
            geom_smooth(method = "loess") +
            theme_classic() +
            labs(
                x = "Natural Log of Coronavirus Cases",
                y = "Natural Log of Weekly Unemployment Claims",
                color = "Month",
                title = "National Unemployment Claims per Week versus Total Coronavirus Cases",
                subtitle = "Grouped across all 50 States, appears to show a strong positive correlation"
            )
        },
        height = 400,
        width = 600
    )
    
    output$plot_scatter_deriv<-renderPlot({
        ggplot(joined_data2, aes(x = log_deriv_cases, y = log_initial_claims, color = date)) +
            geom_point() +
            theme_classic() +
            labs(
                x = "Natural Log of First Derivative of Coronavirus Cases (Effective Growth Rate)",
                y = "Natural Log Weekly Unemployment Claims",
                color = "Month",
                title = "National Unemployment Claims per Week versus Total Coronavirus Cases",
                subtitle = "Grouped across all 50 States, appears to show a strong positive correlation"
            )
        },
        height = 400,
        width = 600
    )
    
}

# ------------------------------
# Running the actual application 
# ------------------------------

shinyApp(ui = ui, server = server)
