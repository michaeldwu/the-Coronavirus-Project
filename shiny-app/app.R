
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


# Loading the necessary libraries

library(shiny)
library(tidyverse)
library(shinythemes)
library(janitor)
library(gt)
library(readr)
library(readxl)
library(ggthemes)
library(lubridate)

# Cleaning Data ---------

untidy_covidata_us <- read.csv('time_series_covid19_confirmed_US.csv') %>%
    clean_names()

covidata_us <- untidy_covidata_us %>%
    pivot_longer(
        cols = starts_with("x"),
        names_to = "date",
        names_prefix = "x",
        values_to = "cases",
        values_drop_na = TRUE
    ) %>%
    select(-uid, -iso2, -iso3, -code3, -fips, -lat, -long, -combined_key) %>%
    rename(county = admin2, country = country_region) %>%
    mutate(date = mdy(as.character(date))) 

unemployment_data_by_state <- read_xlsx("weekly_unemployment_claims.xlsx") %>%
    clean_names() %>%
    mutate(date = mdy(as.character(date))) %>%
    subset(date > "2020-01-05")

# New York ---------------

new_york_covidata <- covidata_us %>%
    filter(province_state == "New York") %>%
    group_by(date) %>%
    summarize(total_cases = sum(cases)) %>%
    mutate(days = 1:93)

new_york_covidata$deriv_cases = c(diff(new_york_covidata$total_cases) / diff(new_york_covidata$days), NA)

new_york_unemployment <- unemployment_data_by_state %>%
    filter(province_state == "New York")


# National Comparison ------

covidata_us_total <- covidata_us %>%
    group_by(date) %>%
    summarize(total_cases = sum(cases)) %>%
    mutate(days = 1:93)

covidata_us_total$deriv_cases = c(diff(covidata_us_total$total_cases) / diff(covidata_us_total$days), NA)

national_unemployment <- unemployment_data_by_state %>%
    group_by(date) %>%
    summarize(total_claims = sum(initial_claims))


# Joined Scatterplot -----

joined_data <- covidata_us %>%
    inner_join(unemployment_data_by_state, by = c("province_state", "date")) %>%
    filter(cases > 0) %>%
    group_by(province_state, date, week, initial_claims) %>%
    summarize(total_cases = sum(cases)) %>%
    mutate(log_total_cases = log(total_cases), log_initial_claims = log(initial_claims))


joined_data2 <- covidata_us %>%
    inner_join(unemployment_data_by_state, by = c("province_state", "date")) %>%
    filter(cases > 0) %>%
    group_by(province_state, date, week, initial_claims) %>%
    summarize(total_cases = sum(cases)) %>%
    mutate(log_total_cases = log(total_cases), log_initial_claims = log(initial_claims))

joined_data2$deriv_cases = c(diff(joined_data2$total_cases) / diff(joined_data2$week), NA)

joined_data2 <- joined_data2 %>%
    mutate(log_deriv_cases = log(deriv_cases))


ui <- fluidPage(navbarPage("the Coronavirus Project",
                           
                           theme = shinytheme("paper"),
                           
                           # First tab, Economy, outputs GDP scatter plot based on checkboxes and line plot based on dropdown selector     
                           
                           tabPanel("National", mainPanel(h4("National Unemployment Claims versus Coronavirus Cases"),
                                                         h4(" "), 
                                                         plotOutput("plot_scatter"), 
                                                         h4("National Unemployment Claims versus Coronavirus Growth"), 
                                                         h4(" "),
                                                         plotOutput("plot_scatter_deriv")
                           )),
                           
                           tabPanel("NY State", mainPanel(h4("New York State Unemployment Claims over Time"),
                                                          h4(" "), 
                                                          plotOutput("plot_ny1"), 
                                                          h4("New York State Coronavirus Growth Rate over Time:"), 
                                                          h4(" "),
                                                          plotOutput("plot_ny2")
                           )),
                           

                           # Fifth tab, About, contains text about data sources and author 
                           
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

# Define server logic

server <- function(input, output) {

    
    # Scatter Plots ----------------------------------------------------------------------------------------------------
    # Creates GDP scatter plot
    
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
        width = 600)
    
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
    width = 600)
    
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
    width = 600)
    
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
    width = 600)

    
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
    width = 600)
    
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
    width = 600)
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
