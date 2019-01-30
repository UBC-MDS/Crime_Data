library(shiny)
library(tidyverse)
library(leaflet)
library(rsconnect)
library(shinythemes)
library(DT)
library(plotly)

# load data
dat <- read.csv("crime_lat_long.csv")

# set crimes for select box input
crimes_list <- c("Homicide" = "homs_per_100k",
                 "Rape" = "rape_per_100k",
                 "Robbery" = "rob_per_100k",
                 "Aggrevated Assault" = "agg_ass_per_100k")

# set crimes for checker box input
crimes_checker <- c("Homicide", "Rape", "Robbery", "Aggrevated Assault")


# get cities for select box input
city_list <- as.list(as.vector(dat$city))

# function to take the nth tick mark
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


# main structure
ui <- fluidPage(
  
  #set theme
  theme = shinytheme("flatly"),
  
  # sed a title
  titlePanel(h1("Violent Crime Rates in the United States", align = "center"),
             windowTitle = "Crime Data"),
  
  # new panel with two tabs
  tabsetPanel(
    # Map tab
    tabPanel(
      title = "Map",
      sidebarLayout(
        # siderbar for map
        sidebarPanel(
          
          sliderInput("year_input", "Select a year",
                      min = 1975, max = 2014, value = 2000, 
                      width = "100%", sep = "", animate = animationOptions(interval = 700, loop = TRUE)),
          checkboxGroupInput("crime_input", 
                             "Select a Crime", 
                             crimes_list, 
                             selected = c("homs_per_100k","rape_per_100k","rob_per_100k","agg_ass_per_100k")
                             )
        ),
        # main panel for map
        mainPanel(leafletOutput("mymap"))
      )
    ),
    # Chart/table tab
    tabPanel(
      title = "Single City",
      sidebarLayout(
        # sidebar for chart, input name changed
        sidebarPanel(
          selectInput("city_input", "Select a city", 
                      selected = as.factor(levels(city_list)[1]), city_list),
          checkboxGroupInput("crime_checks", "Select a Crime", crimes_checker,
                             selected = crimes_checker),
          checkboxInput("pop_check", "Show population", value = FALSE)
        ),
        # main panel
        mainPanel(
          plotlyOutput("line_chart"),
          hr(),
          h4("Comparisions from the National Average and City Safety Ranking",
             align = "center"),
          dataTableOutput("percentage_table")
        )
      )
    ),
    tabPanel(
    title = "Help",
    fluidRow(
      column(3),
      column(6,
             h5("This app allows you to compare violent crime rates from 1975 to 2014 for various cities across the United States.
                The data for this app has been sourced from the Marshall Project and contains population data and violent crimes rates for homicide, rape, robbery, 
                and aggravated assault. With the different tabs you are able to compare national and municipal level data."),
             hr(),
             h4("Map"),
             h5("Use the slide bar to select a single year by sliding it back and forth, you can click the play button for the slider to play through all the years. Each crime type can be selected individually by ticking the checkbox, if multiple boxes are selected the crimes rates will combine to the total crime rate, the default is having all the crimes selected which shows the total crime rate."), 
             hr(),
             h4("Single City"),
             h5("Select a different city from the drop-down menu and control which lines are drawn by selecting the crime checkbox, the default has all crime types selected. The table displays the difference from the national average, the national average used here was calculated from this data set. An overall safety ranking out of 67 based on the total crime rate for that year.") 
             ),
      column(3))
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # get city data for line chart
  single_city_dat <- reactive(
    dat %>% 
      filter(city == input$city_input) %>%
      select(year, total_pop, 
             violent_per_100k, homs_per_100k, rape_per_100k, 
             rob_per_100k, agg_ass_per_100k) %>% 
      mutate(total_pop = total_pop/1000)
  )
  
  # get the boolean for showing the popultion or not
  pop_switch <- reactive(
    input$pop_check
  )
  
  # get city data prepared for plotting lines
  single_city_line <- reactive(
    single_city_dat() %>% 
      rename("Total Violent Crimes" = violent_per_100k,
             "Homicide" = homs_per_100k,
             "Rape" = rape_per_100k, 
             "Robbery" = rob_per_100k, 
             "Aggrevated Assault" = agg_ass_per_100k) %>% 
      gather(total_pop, "Total Violent Crimes", "Homicide", "Rape", 
             "Robbery", "Aggrevated Assault",
             key = "type", value = "count") %>% 
      filter(type %in% c("Total Violent Crimes", input$crime_checks))
  )
  
  # get the city rank for current year
  city_rank <- reactive(
    dat %>% 
      group_by(year) %>%
      mutate(Rank = dense_rank(violent_per_100k)) %>% 
      filter(city == input$city_input)
    )
  
  # get the average for values in table
  avg <- reactive(
    dat %>% 
      group_by(year) %>% 
      summarise(homs = mean(homs_per_100k, na.rm = TRUE),
                rape = mean(rape_per_100k, na.rm = TRUE),
                rob = mean(rob_per_100k, na.rm = TRUE),
                agg = mean(agg_ass_per_100k, na.rm = TRUE))
    )
  
  
  #Get the size for the map circles
  crime_circles <- reactive (
    dat %>%
      filter(year == input$year_input) %>% 
      select(input$crime_input) %>% 
      mutate(calc = rowSums(.,na.rm = TRUE)*ifelse(!("rob_per_100k" %in% input$crime_input) & !("agg_ass_per_100k" %in% input$crime_input),
                                                ifelse("rape_per_100k" %in% input$crime_input,8,20),1))
  )
    
  labs <- reactive(lapply(seq(nrow(dat%>% filter(year == input$year_input))), function(i) {
    z <- dat %>% filter(year == input$year_input)
    paste0( "<ul> <b>",z[i, "city"], "</b><li>", 
            "Total Population: ", prettyNum(round(z[i, "total_pop"]),big.mark = ","),"</li><li>",
            "Total Crime (per 100k): ", prettyNum(round(z[i, "violent_per_100k"]),big.mark = ","),"</li><li>", 
            "Homicide (per 100k): ", prettyNum(round(z[i, "homs_per_100k"]),big.mark = ","),"</li><li>", 
            "Rape (per 100k): ", prettyNum(round(z[i, "rape_per_100k"]),big.mark = ","),"</li><li>", 
            "Robbery (per 100k: ", prettyNum(round(z[i, "rob_per_100k"]),big.mark = ","),"</li><li>", 
            "Assault (per 100k): ", prettyNum(round(z[i, "agg_ass_per_100k"]),big.mark = ","),"</li></ul>" 
            ) 
  }))

  # Map output
  output$mymap <- renderLeaflet({
    leaflet(dat) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~lon, 
                       lat = ~lat, 
                       radius = .008*crime_circles()$calc, 
                       color = "blue",
                       fillOpacity = .01,
                       stroke = FALSE,
                       label = lapply(labs(), HTML)
                       )
  })
  
  # build the line chart
  lines <- reactive(
    ggplot() +
      geom_line(data = single_city_line(), 
                aes(x = year, y = count, color = type), size = 0.5) +
      scale_color_manual(values = c("Aggrevated Assault" = "#a6d854",
                                    "Homicide" = "#fc8d62", 
                                    "Rape" = "#4363d8", 
                                    "Robbery" = "#e78ac3", 
                                    "Total Violent Crimes" = "#e6194b"), name = "") + 
      theme_bw() +
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            legend.text = element_text(size = 8) 
      )+
      scale_x_discrete(limit = c(1975:2015),  breaks = every_nth(n = 5))+
      xlab("Year") + 
      ylab("Crime Rate per 100k People")
  )
  
  # decide whether to add population or not
  final_lines <- reactive(
    if (input$pop_check){
      lines() + 
        geom_bar(data = single_city_dat(), 
                 aes(x = year, y = total_pop),
                 stat = "identity", fill = "slategray1",alpha = 0.5)
    } else {
      lines()
    }
  )
  
  # line chart for trend
  output$line_chart <- renderPlotly(
    final_lines()
  )
  
  # table for percentage
  output$percentage_table <- renderDataTable({
    DT::datatable(single_city_dat() %>% 
      # calculate average compare to national and add % to the end
      mutate(homs_per_100k = paste(round((homs_per_100k - avg()$homs)/100, 
                                         digits = 2), "%"),
             rape_per_100k = paste(round((rape_per_100k - avg()$rape)/100, 
                                         digits = 2), "%"),
             rob_per_100k = paste(round((rob_per_100k - avg()$rob)/100, 
                                         digits = 2), "%"),
             agg_ass_per_100k = paste(round((agg_ass_per_100k - avg()$agg)/100, 
                                         digits = 2), "%"),
             rank = city_rank()$Rank) %>% 
      # clean up table names
      select("Year" = year,
             "Homicide" = homs_per_100k,
             "Rape" = rape_per_100k,
             "Robbery" = rob_per_100k,
             "Aggrevated Assault" = agg_ass_per_100k,
             "Safety Rank" = rank),
      options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

