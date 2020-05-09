# -------------------------------------------
# Shiny Web Application Created by Michael Wu
# Gov 1005 Final Project
# -------------------------------------------

# Loading the necessary libraries

library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)
library(plotly)
library(Cairo)

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
            "Abstract",
             fluidPage(
                 
                 # Adding some basic CSS styling to the page
                 
                 tags$head(tags$style(HTML("h4 {line-height: 1.6; padding-top: 5px; text-align: justify;}"))),
                 tags$head(tags$style(HTML("h2 {padding-top: 10px; padding-bottom: 0px;}"))),
                 
                 br(),
                 imageOutput("banner_image", width = "100%", height = "100%"),
                 br(),
                 fluidRow(
                     column(1), column(10,
                        h2("Background", style = "color: darkred"),
                        h4("Since the United States government starting tracking the metric in January of 1948,
                           we have never experienced a larger single month jump in the unemployment rate than the
                           past month of April 2020. The 10.3% month-over-month increase brings the official
                           unemployment level to 14.7%, a rate that we have not experienced since the peak of
                           the Great Depression – even worse, most academics theorize that the effective unemployment
                           rate of those who have been furloughed and laid off exceeds 20%. In the past seven weeks,
                           33 million Americans have filed for unemployment benefits."),
                        h4("These unprecedented economic hardships but one origin in the COVID-19 pandemic. With 
                           businesses shut down and people mandated to stay at home, economic activity has all but 
                           ground to a halt. Looking forward though, the most critical questions for our economic 
                           recovery remain unclear. How long will this recession last? How much worse will things get?"),
                        
                        h2("Project Objectives", style = "color: darkred"),
                        h4("The answer to these questions can only be uncovered with time, but it’s the objective
                           of this project to help contextualize what we’ve been seeing in the unemployment rate 
                           and the COVID-19 pandemic with respect to data. This project sets out to explore the 
                           effects of the daily reported coronavirus cases on the weekly unemployment claims filed 
                           across all fifty states, searching for strong signs of positive correlation between the 
                           two sets of data. The hope for this project is to be able to uncover a better understanding 
                           of what’s driving the historic rise in unemployment rates across the nation."),
                        
                        h2("Findings", style = "color: darkred"),
                        h4("Put simply, we were able to find a strong positive correlation of 0.8276 between the number 
                           of new daily coronavirus cases and the number of weekly unemployment claims filed for that 
                           week, across the fifty states we studied over the course of the past three months since the 
                           start of the outbreak. What’s especially important to note is that as the growth rate of new 
                           coronavirus cases has slowed in the second half of April, new weekly unemployment claim filing 
                           has accordingly slowed as well. This is an encouraging sign for the economic optimists of the 
                           world, supporting the notion that controlling the coronavirus pandemic is the key to preventing 
                           unemployment (and the rest of the economy) from spiraling out of control."),
                        
                        br(), br(), br()
                    )
                )
        )),
                          
        # Second tab
           
        tabPanel(
            "National",
            fluidPage(
                
                    h2("Strong Positive Correlation between Unemployment Claims and Coronavirus Case Count / Coronavirus Rate of Growth on the National Level",
                       style = "text-align: center; margin-left: 40px; margin-right: 40px; line-height: 1.4;"),
                    h4("Hover over for more details about what each point means!", style = "text-align: center;"),
                    br(), 
                    fluidRow(
                        column(1),
                        column(5, plotlyOutput("plot_scatter", width='100%')),
                        column(5, plotlyOutput("plot_scatter_deriv", width='100%'))
                    ),
                    br(),
                    h4("Displayed above are the two most central plots to this project. On the left hand side displays the 
                       number of new unemployment claims filed by residents of all fifty states over the span of 12 weeks 
                       (this number is not cumulative) plotted with respect to the total number of confirmed COVID-19 cases 
                       (cumulative). In contrast, the right hand side graph displays the unemployment claims plotted against 
                       the growth of rate of the confirmed COVID-19 cases. Both show a strong positive correlation, although 
                       the slope appears to be steeper on the right hand graph.",
                       style = "margin-left: 40px; margin-right: 40px;"),
                    h4("Note: both of these graphs are displayed in log scale for the viewer’s ease. The normal linear scale 
                       graph has a fair number of extremes representing states like New York and California with high populations, 
                       while still displaying a definitive positive correlation and thus presenting the data in log-scale seems preferable.",
                       style = "margin-left: 40px; margin-right: 40px;"),
                    br(),
                    fluidRow(
                        column(1),
                        column(5, includeHTML("gtTables/joined_table1.html")),
                        column(5, includeHTML("gtTables/joined_table2.html"))
                    ),
                    h4("What’s interesting to note is that the correlation coefficients are much stronger for the coronavirus growth 
                       rate compared to the cumulative coronavirus count. Intuitively this makes sense given how the unemployment 
                       claims are not cumulative numbers are would react likely more towards the growth rate of coronavirus cases compared 
                       to a cumulative metric like the total case count.",
                       style = "margin-left: 40px; margin-right: 40px;"),
                    br(), br()
        )),
        
        # Third tab
        
        tabPanel("State by State",
                 fluidPage(
                     h2("Case Study: New York State Unemployment Claims vs Rate Coronavirus Growth",
                        style = "text-align: center; margin-left: 40px; margin-right: 40px; line-height: 1.4;"),
                     br(), 
                     fluidRow(
                         column(1),
                         column(5, plotOutput("plot_ny1")),
                         column(5, plotOutput("plot_ny2"))
                     ),
        )),
        
        # Last 'About' tab, contains informaiton about data sources and the author
        
        tabPanel("More", 
                 fluidPage(
                     
                     # Adding some basic CSS styling to the page
                     
                    tags$head(tags$style(HTML("a {color: #0000CD}"))),
                    tags$head(tags$style(HTML("h4 {line-height: 1.6; padding-top: 5px; text-align: justify; padding-left: 20px; padding-right: 20px;}"))),
                    tags$head(tags$style(HTML("h2 {padding-left: 20px; padding-right: 20px; padding-top: 20px; padding-bottom: 0px;}"))),
                    
                    fluidRow(
                        column(1), column(10,
                              h2("Project Description"),
                              h4("Created as the capstone project for my Harvard Gov 1005 data science class.",
                                 ),
                              h2("Acknowledgements"),
                              h4("I'd like to extend my gratitude towards the", a("Johns Hopkins University Coronavirus Database",
                                 href = "https://github.com/CSSEGISandData/COVID-19"), "for the incredible real-time data
                                 that they provide on their university GitHub page for the general public’s use. Furthermore, this work
                                 would not have been possible without the open-source", a("US Department of Labor’s
                                 Unemployment Database", href = "https://oui.doleta.gov/unemploy/DataDownloads.asp"), ", 
                                 updated every week dating back to 1986 for all fifty states."),
                              h4("I’d also like to thank to my incredible Teaching Fellow Kaneesha Johnson
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
                                 Thank you for visiting!"),
                                          
                              br(), br()
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
                 subtitle = "Data as of May 2nd",
                 x = "Date",
                 y = "Case Count") +
            theme_classic()
        },
        height = 400,
        width = 550
    )
    
    output$plot_ny2<-renderPlot({
        ggplot(new_york_covidata, aes(x = date, y = deriv_cases)) +
            geom_line() +
            geom_smooth(se = FALSE, span = 0.3) +
            labs(title = "Growth of Covid-19 Cases in New York State",
                 subtitle = "Data as of May 8th",
                 x = "Date",
                 y = "First Derivative of Case Count") +
            theme_classic()
        },
        height = 400,
        width = 550
    )
    
    output$plot_scatter<-renderPlotly({
        ggplot(joined_data, aes(x = log_total_cases, y = log_initial_claims, color = date, text = paste("State:", province_state))) +
            geom_point() +
            theme_classic() +
            labs(
                x = "Coronavirus Cases (Log)",
                y = "Weekly Unemployment Claims (Log)",
                color = "Month",
                title = "Unemployment Claims vs Total COVID Cases",
                subtitle = "Grouped across all 50 States, appears to show a strong positive correlation"
            )
        }
    )
    
    output$plot_scatter_deriv<-renderPlotly({
        ggplot(joined_data2, aes(x = log_deriv_cases, y = log_initial_claims, color = date, text = paste("State:", province_state))) +
            geom_point() +
            theme_classic() +
            labs(
                x = "COVID Effective Growth Rate (Log)",
                y = "Weekly Unemployment Claims (Log)",
                color = "Month",
                title = "Unemployment Claims vs COVID Growth Rate",
                subtitle = "Grouped across all 50 States, appears to show a strong positive correlation"
            )
        }
    )
    
}

# ------------------------------
# Running the actual application 
# ------------------------------

shinyApp(ui = ui, server = server)
