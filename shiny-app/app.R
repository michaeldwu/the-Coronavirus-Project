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
national_unemployment <- readRDS("rdsObjects/national_unemployment.rds")
covidata_us_total <- readRDS("rdsObjects/covidata_us_total.rds")
joined_data <- readRDS("rdsObjects/joined_data.rds")
joined_data2 <- readRDS("rdsObjects/joined_data2.rds")

# -------------------------------------------------------------------------
# Front-End UI for the Shiny App, this is the aesthetic skeleton of the app
# Nothing too crazy here, just a few tabs with text and plots on them
# -------------------------------------------------------------------------

ui <- fluidPage(navbarPage("the Coronavirus Project",
                           
                           theme = shinytheme("paper"),
                           
                           # First tab
                           
                           tabPanel("National",
                                    mainPanel(
                                        h4("National Unemployment Claims versus Coronavirus Cases"),
                                        h4(" "), 
                                        plotOutput("plot_scatter"), 
                                        h4("National Unemployment Claims versus Coronavirus Growth"), 
                                        h4(" "),
                                        plotOutput("plot_scatter_deriv")
                           )),
                           
                           # Second tab
                           
                           tabPanel("NY State",
                                    mainPanel(
                                        h4("New York State Unemployment Claims over Time"),
                                        h4(" "), 
                                        plotOutput("plot_ny1"), 
                                        h4("New York State Coronavirus Growth Rate over Time:"), 
                                        h4(" "),
                                        plotOutput("plot_ny2")
                           )),

                           # Last 'About' tab, contains informaiton about data sources and the author
                           
                           tabPanel("About", 
                                    mainPanel(
                                        h4("Project Description"),
                                        p("With the new record levels of unemployment coming out of the US Department of Labor, I figured it would be really interesting to explore how the unemployment claims filed across state vary based on the total number of confirmed cases. I presume that some of the states in the Midwest that have been slower to shut down from the coronavirus will have less claims of unemployment, and I feel like it'd be interesting to see if there's any general correlation."),
                                        h4("Acknowledgements"),
                                        p("A huge thank you to the", a("Johns Hopkins University Coronavirus Database", href = "https://github.com/CSSEGISandData/COVID-19"), "for the incredible real-time data that they provide on their university GitHub page for the general public’s use. This work would also have not been possible without the open-source", a("US Department of Labor’s Unemployment Database", href = "https://oui.doleta.gov/unemploy/DataDownloads.asp"), ", updated every week dating back to 1986 for all fifty states."),
                                        p("I’d also like to give a final thanks to my incredible Teaching Fellow Kaneesha Johnson for helping guide this project throughout its creation and also to the one and only David Kane, Preceptor of Gov 1005 for doing an incredible job acting as the guiding rope to my Ulysses."),
                                        h4("About Me"),
                                        p("My name is Michael Wu and I’m currently a second-year student at Harvard College pursuing a B.A. in Computer Science, Class of 2022. I grew up in sunny Orange County, California and am interested in pursuing change through the startup realm. Outside of school/work, I enjoy playing basketball, poker, watching movies, running, and doing yoga."),
                                        p("If you're interested in learning more about the project or about myself, don't hesistate to reach out through email at mdwu@college.harvard.edu, connect with me on", a("LinkedIn", href = "https://www.linkedin.com/in/michael-d-wu-809522145/"), ", or visit my", a("GitHub Account", href="https://github.com/michaeldwu"), " page. Thanks for visiting!")
                                    ))))

# --------------------------------------------------------------
# Back-end Server Logic for Shiny Apps
# This is where the "plot_output()" functions of the UI tab call
# Creates ggplots with the data objects stored in the RDS files
# --------------------------------------------------------------

server <- function(input, output) {

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
    
    output$plot_national1<-renderPlot({
        ggplot(national_unemployment, aes(x = date, y = total_claims)) +
            geom_line() +
            geom_smooth(se = FALSE, span = 0.4) +
            labs(title = "National Unemployment Filings",
                 subtitle = "As of April 24th",
                 x = "Date",
                 y = "Unemployment Claims") +
            theme_classic()
        },
        height = 400,
        width = 600
    )
    
    output$plot_national2<-renderPlot({
        ggplot(covidata_us_total, aes(x = date, y = deriv_cases)) +
            geom_line() +
            geom_smooth(se = FALSE, span = 0.3) +
            labs(title = "Growth Curve of National Covid-19 Cases",
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
