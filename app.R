#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(scales)
final <- read_csv('combined_data.csv')


library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Energy Insights Tool"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Lighting tiering", tabName = "tiering", icon = icon("check-square-o")),
            menuItem("Budget forecaster", tabName = "budget", icon = icon("dollar"))
        )
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            # First tab content
            tabItem(tabName = "tiering",
                    h2("Street light policy tiering"),
                    div("In this section of the app, planning analysts will choose thresholds on tiering street lights 
                        for usage, safety and other considerations. 
                        This will influence the next step, which is about the policy decisions for these tiers of traffic lights.
                        "),
                    br(),
                    HTML(paste0("Our analysis suggests consideration of street light tiering needs to consider ","<b>","likely foot traffic.","</b>")),
                    div("An important proxy for foot traffic is public transport."),
                    br(),
                    div("The other major considerations identified include parks and tourist attractions.
                        Planning analysts can choose the importance of the the latter considerations given the declining foot traffic in these locations"),
                    br(),
                    div("To make our decisions easy, a traffic light system is proposed. Red street lights will be subject to the most cost minimisation measures,
                        and green street lights are essential for public safety, so any opportunities to reduce long term cost will be considered."),
                    br(),
                    fluidRow(
                        box(
                            title = "Safety tier controls",
                            "The minimum value determines 'red' tiers, the maximum value determines 'green' tiers.",
                            br(),
                            br(),
                                sliderInput("bus_slider", "Minimum number of bus stops nearby (200m), or:", 0, 10, c(2,4)),
                                sliderInput("park_slider", "Number of parks nearby (200m), or:",0,5,c(0,1)),
                                sliderInput("tourist_slider", "Number of tourist destinations nearby (200m) :",0,5,c(0,1)),
                            width = 3,height = 500
                        ),
                        box(leafletOutput("map", height = 500),width=9),
                    br(),
                    h2("Policy setting"),
                    div("In this section planning experts will set the policy based on each of the street light tiers above"),
                    br(),
                    fluidRow(
                        box(
                            title = "Red tier policy",
                            "Red tiers have the least trade off to public safety, costs should be minimised",
                            br(),
                            selectInput("red_policy","Red policy", choices = c("Dim lights","Replace with Solar","Replace with LED + metered"),
                            selected = "Dim lights"),
                            width = 4
                        ),
                        box(
                            title = "Amber/Yellow tier policy",
                            "Amber policy should be less efficiency and cost saving driven than the red tier",
                            br(),
                            selectInput("amber_policy","Amber policy", choices = c("Dim lights","Replace with Solar","Replace with LED + metered"),
                            selected = "Replace with LED + metered"),
                            width = 4
                        ),
                        box(
                            title = "Green tier policy",
                            "Green tiers should consider long term benefits",
                            br(),
                            selectInput("green_policy","Green policy", choices = c("Dim lights","Replace with Solar","Replace with LED + metered"),
                            selected = c("Replace with Solar")),
                            width = 4
                        ),
                        br()
                        
                    )
                )
            ),
            tabItem(tabName = "budget",
                    h2("Budget forecaster"),
                    div("This section displays the approximate budget implications on decisions and how they compare to Hobart City's budget"),
                    div("It is assumed that each treatment option has an initial cost, and ongoing benefit, except for dimming lights 
                        which are assumed to be controlled from electricity providers to reduce power to pole."),
                    br(),
                    h4("Cost assumptions"),
                    br(),
                    div("The cost assumptions are as follows: "),
                    div("Dim lights: $0.03 per kWh saved (10% of $0.40 per kwh of total cost reported by CoH)"),
                    br(),
                    div("Replace with LED + metered:  Electricity savings are calculated at the rate of the difference in hours between 3650 hours (365 days * 10 hours) and annualised, monthly averaged 
                        time between sunset and sunrise (3090 hours) in Hobart (Geoscience Australia). In addition, the difference between the wattage of current light and 18 watts. 
                        Budget savings are calculated at rate of  $0.40 per kWh saved (Sourced from supplied dataset on upgrade costs)"),
                    div("Replace with Solar:  Electricity savings are calculated at 3650 hours (365 days * 10 hours) at $0.40 per KWh. Lighting installation costs are calculated at 
                         $8100 (Figures from data on costs supplied by CoH).
                        Note in any scenario we don't propose to change lights if there are existing LED lights installed."),
                    h4("Overall break even position over time"),
                    box(plotOutput("total_plot"),width=12),
                    
                    box(plotOutput("cost_plot"),width=12)
                    )
    )
    )
)


server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    new_output <- reactive({final %>% 
        mutate(classes = case_when(bus_stops_nearby >= input$bus_slider[[2]]  | park_nearby > input$park_slider[[2]] | tourist_nearby > input$tourist_slider[[2]] ~ 2,
                                   bus_stops_nearby >= input$bus_slider[[1]] | park_nearby > input$park_slider[[1]] | tourist_nearby > input$tourist_slider[[1]] ~ 1,
                                   TRUE~0))})
    

    budget <- reactive({
                new_output() %>% 
                mutate(treatment = case_when(classes == 0 ~ input$red_policy,
                                     classes == 1 ~ input$amber_policy,
                                     classes == 2 ~ input$green_policy),
               fixed_cost = case_when(treatment=="Replace with Solar" & wattage > 100 ~ -8100,
                                      treatment=="Replace with LED + metered" & wattage > 100 ~ -700,
                                      TRUE~0),
               variable_benefit = case_when(treatment=="Replace with Solar" & wattage > 100 ~ 0.4*(wattage/1000)*3650,
                                            treatment=="Replace with LED + metered" & wattage > 100 ~ ((3650-3090)*0.4*(wattage/1000) + (wattage-18+5)/1000*0.4),
                                            treatment == 'Dim lights' ~ 0.04*((wattage+0.00001)/1000)*3650,
                                            TRUE~0)) %>% 
                group_by(treatment) %>% 
                summarise(sum_fixed_cost = sum(fixed_cost,na.rm=TRUE),
                sum_variable_benefit = sum(variable_benefit,na.rm=TRUE)) %>% 
                mutate(newcol = 1) })


    simulation <- reactive({
        as.data.frame(seq(from=2020,to=2080))%>% rename('Year' = 'seq(from = 2020, to = 2080)') %>% 
        mutate(newcol = 1) %>% left_join(budget()) %>% arrange(treatment,Year) %>% 
        mutate(year_total = ifelse(Year == 2020,sum_fixed_cost + sum_variable_benefit,sum_variable_benefit)) %>% 
        group_by(treatment) %>% mutate(cumulative=cumsum(year_total))
    })
    
    total_budget_impact <- reactive({simulation() %>% group_by(Year) %>% summarise(total_cost = sum(year_total)) %>% 
        mutate(cumulative=cumsum(total_cost))})
    
    output$cost_plot <- renderPlot({ ggplot(simulation(),aes(y=cumulative,x=Year))+ geom_line() +  facet_wrap(vars(treatment), scales = "free") + 
        scale_y_continuous(name="Running budget total", labels = comma) + 
        ggtitle('Break even analysis by time and policy') + 
        theme_bw() + 
        geom_hline(yintercept = 0, color = "red", size=1.5)})
    
    output$total_plot <- renderPlot({
        ggplot(total_budget_impact(),aes(y=cumulative,x=Year))+ geom_line() +
            scale_y_continuous(name="Running budget total", labels = comma) + 
            ggtitle('Break even analysis by time, overall') + 
            theme_bw() + 
            geom_hline(yintercept = 0, color = "red", size=1.5)
    })
    
    pal <- colorNumeric(
        palette = "RdYlGn",
        domain = c(2,1,0))
    
    output$map <- renderLeaflet({leaflet(new_output())%>% addTiles() %>%
        addCircles(lng = ~Longitude, lat = ~Latitude, color = ~pal(classes),fillOpacity = 1
        )})
    
    
}


shinyApp(ui, server)
