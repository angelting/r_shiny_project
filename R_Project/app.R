library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(magrittr)
library(leaflet)



stations_df <- read.csv(file="Data/__Stations.csv", header=TRUE, sep=",")
filelist <- (list.files(path = "Data"))[-1]   ## -1 to remove __stations.csv from list

### Based on files in the directory, create columns of cities and available pollutants
## Pollutants are the 6th character
pollutant_station  <- matrix(nrow=0,ncol=2) 
for (i in filelist){
    pollutant = sub(".csv","", substring(i, first=9))
    station = substr(i,start=1, stop=7)    
    pollutant_station <- rbind(pollutant_station, c(pollutant, station))    
}

pollutant_station <- as.data.frame(pollutant_station)
names(pollutant_station) <- c("pollutant", "station_code")



aggregation <- c("Raw Hourly Data",
                 "Daily Averages",
                 "Daily Maxima",
                 "Number of Hours per Day Over Threshold",
                 "Number of Hours Per Year Over Threshold", 
                 "Number of Days Per Year Over Threshold")
time_axis_choices <- c("Calendar Time", "Day of the Year","Day or Hour of the Week", "Hour of the Day")
date_format<-c("CalendarTime","%m-%d","%A", "%H")
date_df <- as.data.frame(cbind(time_axis_choices, date_format))


calendartime_plot <- function(df,aggregation_type, threshold=0){
  if (aggregation_type == "Daily Averages"){
    df2 <- df %>% group_by(calendartime = decimal_date(date), station_name) %>% summarise(daily_average=mean(Concentration))
    x <- df2$calendartime
    y <- df2$daily_average
    x_lab <- "Date"
    y_lab <- "Daily Average"
    title <- "Daily Average by Date"
    
  }
  
  else if (aggregation_type == "Raw Hourly Data"){
    df2 <- df %>% mutate(calendar_time = decimal_date(date))
    x <- df2$calendar_time
    y <- df2$Concentration
    x_lab <- "Date"
    y_lab <- "Concentration"
    title <- "Hourly Pollutant Concentration"
    
  }
  else if (aggregation_type == "Daily Maxima"){
    df2 <- df %>% group_by(calendartime = decimal_date(date), station_name) %>% summarise(daily_max=max(Concentration))
    x <- df2$calendartime
    y<-df2$daily_max
    x_lab <- "Date"
    y_lab <- "Daily Maxima"
    title <- "Daily Maxima by Date"
  } 
  else if (aggregation_type == "Number of Hours Per Year Over Threshold"){
    df2 <- df %>% mutate(hour = format(date, "%H"), year=format(date,"%Y"))%>% filter(Concentration>threshold) %>% group_by(station_name, year) %>% summarise(number_of_hours = n())
    x <- df2$year
    y<-df2$number_of_hours
    group <- df2$station_name
    x_lab <- "Year"
    y_lab <- "Hours"
    title <- "Number of Hours per Year when a given threshold is exceeded"
  }
  else if (aggregation_type =="Number of Hours per Day Over Threshold"){
    df2 <- df %>% mutate(hour = format(date, "%H"), day=ymd(format(date, "%y-%m-%d")))%>% filter(Concentration>threshold) %>% group_by(station_name, day) %>% summarise(number_of_hours = length(hour))
    x <- df2$day
    y<-df2$number_of_hours
    x_lab <- "Date"
    y_lab <- "Hours"
    title <- "Number of Hours per Day when a given threshold is exceeded"
  }
  else if (aggregation_type == "Number of Days Per Year Over Threshold"){
    df2 <- df %>% mutate(day=ymd(format(date, "%y-%m-%d")))%>% 
      group_by (station_name, day) %>% 
      summarise(daily_concentration = mean(Concentration)) %>%
      filter(daily_concentration>threshold) %>% group_by(station_name, year = format(day, "%Y")) %>% 
      group_by(station_name,year) %>% summarise(number_of_day = length(day))
    
    x <- df2$year
    y<-df2$number_of_day
    x_lab <- "Year"
    y_lab <- "Daily average concentration exceeds threshold (Days per year)"
    title <- "Number of days per year for which the daily average concentration exceeds a given threshold"
  }
  group <- df2$station_name
  plot<-ggplot(data=df2, aes(x=x,y=y, group=group))+
    geom_line(colour=group)
    xlab(x_lab)+
    ylab(y_lab)+
    ggtitle(label=title)
  return (plot)
}
scatter_plot <- function(date_type, df, aggregation_type, threshold=0, x_label){
  if (aggregation_type == "Daily Averages"){
    
    df2 <- df %>% group_by(calendartime = format(date, date_type), station_name) %>% summarise(daily_average=mean(Concentration))
    x <- df2$calendartime
    y <- df2$daily_average
    title <- "Daily Average by Date"
  }
  
  else if (aggregation_type == "Raw Hourly Data"){
    df2 <- df %>% mutate(calendar_time = format(date, date_type))
    x <- df2$calendar_time
    y <- df2$Concentration
    title <- "Hourly Pollutant Concentration"
    
  }
  else if (aggregation_type == "Daily Maxima"){
    df2 <- df %>% group_by(calendartime = format(date, date_type), station_name) %>% summarise(daily_max=max(Concentration))
    x <- df2$calendartime
    y<-df2$daily_max
    title <- "Daily Maxima by Date"
  } 
  
  else if (aggregation_type =="Number of Hours per Day Over Threshold"){
    df2 <- df %>% mutate(hour = format(date, "%H"), day=format(date, date_type))%>% filter(Concentration>threshold) %>% group_by(station_name, day) %>% summarise(number_of_hours = length(hour))
    x <- df2$day
    y<-df2$number_of_hours
    title <- "Number of Hours per Day when a given threshold is exceeded"
  }
  group<-df2$station_name
  
  ggplot(df2, aes(x=x, y=y, group=group, color=group))+geom_point(alpha = 0.2,)+
    
    ylab(aggregation_type)+
    ggtitle(label=title)
}





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Air Quality of Czech Republic from 2013 - 2019"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "pollutant",
                        label = "Choose a pollutant type:",
                        choices = c("PM2.5", "PM10", "NO2", "SO2"),
                        selected="PM2.5"
                        ),
            
            htmlOutput(outputId = "station_names"),
            actionButton(inputId="data_load", "Load Data"),
            
            
            hr(),

     
            selectInput(inputId="aggregation_type",
                        label = "Choose an aggretation type: ",
                        choices=aggregation,
                        multiple=FALSE),
            
            htmlOutput(outputId = "threshold"),
            htmlOutput(outputId = "date_axis"),
         
  
            actionButton(inputId="viz_data", "Visualise Data"),
            

        ),
    
        


        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            h3("Stations Location"),
            leafletOutput("czechmap"),
            h3("Visualisation"),
            plotOutput("plot_result")
            
            
          ),
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### returns station names based on pollutant chosen
  pollutant_type<-reactive({input$pollutant})

  output$station_names <-renderUI({
    station_codes <- (pollutant_station %>% filter(pollutant==pollutant_type()) %>%select(station_code))$station_code
    station_name <- stations_df %>% filter(EoICode %in% station_codes)%>%select(StationName)
    selectizeInput(inputId="station_names_selector",
                label = "Choose up to 4 station names: ",
                choices = station_name,
                multiple=TRUE,
                options = list(maxItems = 4))
    })
  
  station_names<-reactive({input$station_names_selector})
  
  output$czechmap <- renderLeaflet({
    
    if (is.null(station_names)==FALSE){
      stations_geo <- stations_df %>% filter(StationName %in% station_names())%>%select(StationName, Latitude, Longitude)
      map <- leaflet(stations_geo) %>% 
        addTiles() %>% setView(lng=14.418540, lat=50.073658, zoom = 7)%>%
        addMarkers(~Longitude, ~Latitude, popup=~StationName, labelOptions = labelOptions(noHide = T))
      
    }
    map
  })
  
  
  
  ### query data based on pollutant+station code.csv 
  pollutant_df <- eventReactive(input$data_load,{
    pollutant_df  <- matrix(nrow=0,ncol=8) 
    for (i in station_names()){
      EoICode <-(stations_df %>% filter(StationName==i)%>%select(EoICode))$EoICode
      fileName <- paste0("Data/", EoICode,"_", pollutant_type(),".csv")
      loadcsv <- read.csv(file=fileName, header=TRUE, sep=",")
      loadcsv <- cbind(loadcsv, i)
      pollutant_df <- rbind(pollutant_df, loadcsv)
      
    }
    names(pollutant_df)[8]<-"station_name"
    pollutant_df <- pollutant_df %>% drop_na()
    
    pollutant_df$date <- paste(pollutant_df$Year,
                               pollutant_df$Month,
                               pollutant_df$Day,
                               pollutant_df$Hour,
                               sep="-") %>% ymd_h() 
    
    
    
    
    pollutant_df
    
  })
  
  ### returns threshold slider given a chosen aggregation type
  output$threshold <-renderUI({
   
    if (input$aggregation_type %in% c("Daily Averages","Daily Maxima","Raw Hourly Data")){
    }
    else{
      sliderInput(inputId="threshold",
                  label = "Slide to select a threshold",
                  value=50, ## update so that it is median
                  min=0,
                  max=100, ### need to update it so that it decide based on the file selected
      )}
  })
  
  output$date_axis <- renderUI({
    if (input$aggregation_type %in% c("Number of Hours Per Year Over Threshold", "Number of Days Per Year Over Threshold")){
      choices = time_axis_choices[1]

    }
    else{
      choices = time_axis_choices
      }
    selectInput(inputId="timeline_x_axis",
                label="Choose a x-axis to represent time: ",
                choices = choices)
  })
  
  ### return plot when visualise data button is run
  apply_plot <-eventReactive(input$viz_data, {

    if (input$timeline_x_axis == "Calendar Time"){
      if(is.null(input$threshold)==TRUE){
        plot_result <- calendartime_plot(pollutant_df(), input$aggregation_type)
      }
      else{
        
        plot_result <- calendartime_plot(pollutant_df(), input$aggregation_type, input$threshold)
        }
    }
    else{
      
      date_type <- (date_df %>% filter(time_axis_choices == input$timeline_x_axis) %>% select(date_format))[,1] %>% as.character

      if(is.null(input$threshold)==TRUE){
        plot_result <- scatter_plot(date_type,pollutant_df(), input$aggregation_type, x_label=input$timeline_x_axis)
      }
      else{
        
        plot_result <- scatter_plot(date_type, pollutant_df(), input$aggregation_type, input$threshold,x_label=input$timeline_x_axis)
      }
    }
    plot_result
  })
  
  ## plots rendering
  output$plot_result <-renderPlot({
    apply_plot()
  })
  


}

# Run the application 
shinyApp(ui = ui, server = server)
