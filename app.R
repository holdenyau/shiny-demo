#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
# Load packages
library(shiny)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(shinydashboard)
library(forecast)
library(rsconnect)
library(shiny)
library(dplyr)
library(forecast)
library(stringr)
library(scales)
library(readr)
library(DT)
library(readr)
library(data.table)
# Load data



gap <- data.table::fread("d:/r/haocheng/haochengqiu/shinygap1.csv",encoding = "UTF-8")

ui <- dashboardPage(
    dashboardHeader(title = "Data Search"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Daily Data",tabName = "daily",icon = icon("daily")),
            menuItem("Monthly Data",tabName = "monthly",icon = icon("monthly")),
            menuItem("Annual Data",tabName = "annual",icon = icon("annual")),
            menuItem("personal report", tabName = "report", icon = icon("report"))
        )
    ),
    dashboardBody(
        tabItems(
            #First tab content
            tabItem(tabName = "daily",
                    fluidPage(
                        # select district 
                        box(title = "holden", width = 4,status = "primary",solidHeader = TRUE,
                            selectInput("District3","Select a district",
                                        levels(as.factor(gap$District))),
                            uiOutput("secondSelection3"),
                            # select data range
                            dateRangeInput("date",strong("Date range"),
                                           start = "2016-02-07",end = "2020-03-31",
                                           min = "2016-02-07",max = "2020-03-31"),
                            # filter sales
                            sliderInput("sales", "Select sales range",value = c(0,max(gap$Net.Sales.Amt,na.rm = TRUE)),dragRange = TRUE,
                                        min = 0,max = max(gap$Net.Sales.Amt,na.rm = TRUE)))
                    )
                    
            ), 
            tabItem(tabName = "monthly",
                    fluidPage(
                        tabBox(id = "top10",
                               tabPanel("Top 10 Sales",
                                        dateRangeInput("month",strong("Date range1"),
                                                       start = "2016-02-07",end = "2020-03-31",
                                                       min = "2016-02-07",max = "2020-03-31"),
                                        plotOutput("top10s"),
                                        tableOutput("table")
                               ),
                               tabPanel("Top 10 Growth",
                                        selectInput("month1","Select month:",
                                                    levels(as.factor(gap$Month))),
                                        plotOutput("top10g")
                               )
                        ))),
            tabItem(tabName = "annual",
                    fluidRow(
                        tabBox(
                            title = "Select",
                            id = "tabselect",
                            tabPanel("Compare Option",
                                     selectInput("compare","Compare Option:",
                                                 c("Single","Compare"))),
                            tabPanel("1",
                                     selectInput("year","Year:",
                                                 unique(gap$Year)),
                                     selectInput("District","Select a district",
                                                 levels(as.factor(gap$District))),
                                     uiOutput("secondSelection")),
                            tabPanel("2",
                                     selectInput("year2","Year:",
                                                 unique(gap$Year)),
                                     selectInput("District2","Select a district",
                                                 levels(as.factor(gap$District))),
                                     uiOutput("secondSelection2"))
                            
                        )
                    ),
                    fluidRow(
                        tabBox(
                            title = "Sales trend",
                            id = "tabtrend",height = "250px", width = "400px",
                            tabPanel("Monthly",
                                     plotOutput("distplot",height = 500,width = 700)),
                            tabPanel("Daily",
                                     plotOutput("dailyplot",height = 300,width = 1000))
                        ),
                    )
            ),
            tabItem(tabName = "report",
                    downloadButton("down1","下载rmarkdown的html报表"))
        )
    )
)
server <- function(input,output){
    list <- reactive({
        gap %>%
            mutate(Month = as.factor(Month)) %>%
            group_by(Year,Month,store.name,Store.Name..EN.,District) %>%
            dplyr::summarise(sales = mean(Net.Sales.Amt,na.rm = TRUE)) 
    })
    
    output$secondSelection <- renderUI({
        selectInput(inputId = "store",
                    label = "Select a store:",choices = filter(list(),District == input$District & Year == input$year)$store.name)
    })
    output$secondSelection2 <- renderUI({
        selectInput(inputId = "store2",
                    label = "Select a store:",choices = filter(list(),District == input$District2 & Year == input$year2)$store.name)
    })
    output$secondSelection3 <- renderUI({
        selectInput(inputId = "store3",
                    label = "Select a store:",choices = filter(list(),District == input$District3)$store.name)
    })
    
    
    data <- eventReactive(input$store,
                          mydata <- gap %>%
                              mutate(Month = as.factor(Month)) %>%
                              group_by(Year,Month,store.name,Store.Name..EN.,District) %>%
                              dplyr::summarise(sales = mean(Net.Sales.Amt,na.rm = TRUE)) %>%
                              filter(Year == input$year) %>%
                              filter(store.name == input$store)
    )
    
    
    data2 <- eventReactive(input$store2,
                           mydata2 <- gap %>%
                               mutate(Month = as.factor(Month)) %>%
                               group_by(Year,Month,store.name,Store.Name..EN.,District) %>%
                               dplyr::summarise(sales = mean(Net.Sales.Amt,na.rm = TRUE)) %>%
                               filter(Year == input$year|Year == input$year2 ) %>%
                               filter(store.name == input$store|store.name == input$store2)
    )
    
    dailydata <- eventReactive(input$store,
                               mydata3 <- gap %>%
                                   filter(Year == input$year) %>%
                                   filter(store.name == input$store)
    ) 
    
    
    dailydata2 <- eventReactive(input$store2,
                                mydata4 <- gap %>%
                                    filter(Year == input$year|Year == input$year2 ) %>%
                                    filter(store.name == input$store|store.name == input$store2)
    ) 
    
    dailydata3 <- eventReactive(input$store3,
                                mydata5 <- gap %>%
                                    filter(store.name == input$store3))
    
    output$distplot <- renderPlot({
        ggplot( if (input$compare == "Single"){
            data = data()
        } else {data = data2()
        })+
            geom_bar(aes(x = Month ,y = sales,fill = Store.Name..EN.),
                     stat = "identity",position = "dodge",width = 0.6)+
            xlab("month")+
            ylab("Sales  (per day)")+
            scale_y_continuous(expand = c(0,0))+
            theme(
                panel.background = element_rect(fill = "white"),
                axis.text = element_text(size = 9),
                panel.grid.major.y = element_line(alpha("black",alpha = 0.3))
            )
    })
    
    output$dailyplot <- renderPlot({
        ggplot(if (input$compare == "Single"){
            data = dailydata()
        } else {data = dailydata2()
        })+
            geom_line(aes(x = as.POSIXct(as.character(Date)), y = Net.Sales.Amt,color = Store.Name..EN. ))+
            scale_x_datetime(date_breaks = ("30 days"),labels = date_format("%m"))+
            xlab("Month")+
            ylab("Sales")+
            theme(
                axis.text = element_text(size = 9),
                panel.background = element_rect(fill = "white"),
                panel.grid.major.y = element_line(alpha("black",alpha = 0.3))
            )
    })
    
    output$down1 <- downloadHandler(
        filename = function() {
            paste('Data-summary', Sys.time(), sep = '.', 'html')
        },
        content = function(file) {
            src <- normalizePath('learning.Rmd')
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'learning.Rmd', overwrite = TRUE)
            library(rmarkdown)
            out <- render('learning.Rmd', html_document())
            file.rename(out, file)
        })

}

shinyApp(ui = ui, server = server)



