
library(readr)
library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus) 
library(bs4Dash)
library(leaflet)
library(DT)
library(plotly)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(geojsonio)
library(tidyjson)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(tidyverse) #For reading the file from a txt file and for the %>% operator
library(tidyr)




nhe <- read_csv("NHE.csv")
str(nhe)
nhe$Medicare <- as.numeric(nhe$Medicare)
nhe$Medicaid <- as.numeric(nhe$Medicaid)
######### ISAAC
insurance <- read_csv("insurance.csv")
gender <- insurance %>% distinct(sex) %>% pull(sex)
children <- insurance %>% distinct(children) %>%
    arrange(children, desc(children)) %>%
    pull(children)
insurance = insurance %>%
    mutate(bmi_label = case_when(bmi>=40~"morbidly obese",
                                 bmi>=30~ "obese",
                                 bmi>25~"overweight",
                                 bmi>=18.5~"healthy",
                                 bmi<18.5~"underweight"
    ))
bmi_label_list = c("morbidly obese","obese","overweight","healthy","underweight")
age <- insurance %>%
    mutate(age_range = case_when(age<=40~"18-40",
                                 age>=41~"41-64"))
age_list = c("18-40", "41-64")
############ END of ISAAC

Color = c("YlOrRd", "Greens", "Blues")
Year = c("19", "18", "17")
mapd <- read_csv("md_all.csv")

### Gets file From http://leafletjs.com/examples/choropleth/us-states.js
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
### remove puerto rico because CMS data does not have it
states <- states[-52,]  

ui <- dashboardPage(
      dashboardHeader(title = "Healthcare Costs"),
      dashboardSidebar(
          minified = FALSE,
          collapsed = FALSE,
          sidebarMenu(
              menuItem("Home", tabName = "page1_Home", icon = icon("info")),
              menuItem("Cost by Demographics", tabName = "page2", icon = icon("area-chart")),
              menuItem("Costs by State", tabName = "page3_leaflet", icon = icon("area-chart")),
              menuItem("Hospital Procedures", tabName = "page4_word_cloud", icon = icon("map-o")),
              menuItem("National Expenditure",tabName = "page5_NHE",icon=icon("database"))
          )
      ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1_Home",
                     fluidRow(
                       box(
                         title = "Introduction",
                         # align="center",
                         solidHeader = TRUE,
                         status = "success",
                         width = 12,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         column(
                           12,
                           tags$div(
                             tags$span(
                               p("The cost of healthcare is a hot topic for beneficiaries, payers, providers and other stakeholders in the 
                        healthcare industry. With various sectors, stakeholders, and environmental factors impacting and influencing
                        each other, and the lack of availability of service charge data, it's important to provide a resource that 
                        may provide insight to what a beneficiary may pay for service(s)."),
                        p("This app will provide users with the ability to view discharge amounts billed from hospitals across four 
                        regions by demographics, as well as a map showing the user the following", tags$b("averages")," per state:"),
                        tags$li("Number of patients discharged"),
                        tags$li("Submitted bill"),
                        tags$li("Payment"),
                        tags$li("Medicaid payment")
                             ),
                        style = "font-size:14px"
                           )
                         )
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Why Healthcare Costs?",
                         solidHeader = TRUE,
                         status = "info",
                         width = 12,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         column(
                           12,
                           tags$div(
                             tags$span(
                               p("The purpose of this app is for exploratory purposes. Individuals interested in exploring overall healthcare 
                      costs can find this information based on several factors including age, gender, and BMI."),
                      p("We wanted to explore the healthcare costs billed to beneficiaries and used data from CMS and Kaggle to 
                      explore two different datasets."),
                      p(tags$b("1. Medical costs personal dataset")),
                      tags$li('This dataset comes from the book, "Machine Learning with R" by
                        Brett Lantz. The data highlights the individual medical cost billed by an insurer for a beneficiary. It
                        includes beneficiary information such as age, sex, BMI, smoking status, number of children, and
                        regional location.'),
                      br(),
                      p(tags$b("2. Hospital charges and Medicare Payments by Geography")),
                      tags$li("This dataset provides information on overall charges, payments, and Medicare payments for each state.")
                             )
                           )
                         )
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Meet the Team",
                         solidHeader = TRUE,
                         status = "primary",
                         width = 12,
                         collapsible = TRUE,
                         collapsed = TRUE,
                         column(
                           12,
                           tags$div(
                             tags$span(
                               p("Haitham Al-Grain"),
                               tags$li("Assistant Professor of Anesthesiology and Critical Care Medicine at Johns Hopkins University"),
                               tags$li("Undergraduate from University of Maryland, College Park"),
                               tags$li("Medical degree from George Washington University in D.C."),
                               tags$li("Currently pursuing an MBA from Johns Hopkins University"),
                               br(),
                               p("Isaac Lee"),
                               tags$li("Trader at an investment firm"),
                               tags$li("Undergraduate from California State University, Fullerton"),
                               tags$li("Currently pursuing a MSc of Finance at Johns Hopkins University"),
                               br(),
                               p("Katia Fortune"),
                               tags$li("Currently a full time Senior Analyst for CareFirst BlueCross BlueShield."),
                               tags$li("Part time student in the Masters of Business Administration (MBA) program at Johns Hopkins Carey Business School, with a concentration in Business Analytics and Risk Management, Healthcare Management, Innovation and Technology and Digital Marketing."),
                               tags$li("Background in Health Policy, Healthcare Compliance and Federal Healthcare Policy."),
                               br(),
                               p("Phuong Nguyen"),
                               # tags$li(""),
                               # tags$li(""),
                               # tags$li(""),
                               br(),
                               p("Rithvik Reddy")
                               # ,
                               # tags$li(""),
                               # tags$li(""),
                               # tags$li("")
                             ),
                             style = "font-size:14px"
                           )
                         )
                       )
                     )
                    ),                   
            tabItem(tabName = "page2"
                    ,fluidPage(
                        box(
                            title = "Instructions",
                            solidHeader = TRUE,
                            status = "secondary",
                            width = 12,
                            collapsible = FALSE,
                            column(
                                12,
                                tags$div(
                                    "Please select your gender, number of children, BMI category, and age group below, and check the box 
                      if you are a smoker to find out the average healthcare cost depending on your region."
                                )
                            )
                        ),
                      fluidRow(
                          column(4, 
                                 selectInput(
                                     "sex",
                                     label = h6("Select Gender"),
                                     choices = gender),
                                 
                                 selectInput(
                                     "children",
                                     label = h6("Number of Children (Max is 5)"),
                                     choices = children),
                                 
                                 selectInput(
                                     "bmi",
                                     label = h6("BMI Category"),
                                     choices = bmi_label_list),
                                 
                                 selectInput(
                                     "age",
                                     label = h6("Age"),
                                     choice = age_list
                                 ),
                                 
                                 checkboxInput("smoker", label = "Smoker", value = FALSE)
                          ),
                          
                          
                          column(8, plotOutput("plot1", height = 500)
                          ))
                    )
            ),
            # , sliderInput("year", "sliderInput", min = 2014, max = 2020, value = 1,
            #   step = 1, animate = animationOptions(interval = 2000, loop = FALSE))            
            tabItem(tabName = "page3_leaflet",
                    fluidRow(
                        box(align="center",
                            title = "Details",
                            align="center",
                            solidHeader = FALSE,
                            status = "orange",
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            column(
                                12,
                                tags$div(
                                  tags$span(
                        p("This app will provide users with the ability to view average number of discharged patients and bills submitted by hospitals, per state:"),
                        tags$li("Number of patients discharged"),
                        tags$li("Submitted bill"),
                        tags$li("Payment"),
                        tags$li("Medicaid payment")
                                  ),
                        style = "font-size:14px"
                            )
                        )
                        )
                    ),
                    fluidRow(column(width = 6, 
                                    selectInput(
                                        "Year",
                                        label = h4("Select Year"),
                                        choices =Year
                                    )) ,    
                             (column(width = 6
                                     # ,selectInput(
                                     #     "Color_Scale",
                                     #     label = h2("Select Color Scale"),
                                     #     choices =Color_Scale)
                                     ))),
                    # h2("2019"),
                    # h2(paste0("20",Year_ui)),
                    br(),
                    fluidRow(column(width = 12,
                                    h2("Charges Submitted"),
                                    align="center",
                                    leafletOutput("md_19", height = 400))),
                    br(),
                    fluidRow(
                        column(width = 6,
                                    h4("Number of Discharged Patients"),
                               align="center",
                               leafletOutput("md_18", height = 300)),
                             (column(width = 6,
                                    h4("Population Density"),
                                    align="center",
                                leafletOutput("md_17", height = 300)))
                    )#fluidrow
            ),
            tabItem(tabName = "page4_word_cloud",
                    # fluidRow("The Many Meanings of a Single Word",
                    #         style="font-size:18px; background-color:#ac8f8f", align="center"),
                    fluidRow(column(width=12,
                                    h2("Hospital Medicare Average Payments"), ######## Q:
                                    h5("Select Payment Range", style="font-size:11px"),
                                    style = "text-align: center",
                                    br(),
                                    sliderInput("range","Average Charge", min =500, max = 1000, value = c(300, 900)),
                                    div(verbatimTextOutput("out1"), style = "width: 300px;"),
                                    sliderInput("max",
                                                "Maximum Number of Charges",
                                                min = 20,  max = 300,  value = 60)
                    )
                    ),#fluidrow
                    fluidRow(column(width=12,
                                    "Hospital Services",
                                    br(),
                                    plotOutput("wcoutput",height = 500,width = "100%"),
                                    # wordcloudOutput("wcoutput", width="100%"),
                                    br(),
                                    br(),
                                    br(),
                                    a("Data Source",href="https://www.cms.gov/",target="_blank")
                                    
                    )
                    )#fluidrow
            ),
            tabItem(tabName = "page5_NHE",
                    sliderInput("year", "Year:", min = 1960, max = 2030, value = c(1960, 2030)),
                    plotOutput("plot_NHE")
            )
        )
    )## dashboardBody
)## dashboardPage

server <- function(input, output, session) {
    
    
    output$plot1 = renderPlot({
        
        if(input$smoker){
            insurance1 = insurance %>%
                filter(smoker=="yes")
        } else{
            insurance1 = insurance 
        }
        
        
        if(input$age == "18-40"){
            insurance1 = insurance1 %>%
                filter(age <=40 & age>=18)
        } else if(input$age == "41-64"){
            insurance1 = insurance1 %>%
                filter(age <=64 & age>40)
        }
        
        return(
            insurance1 %>%
                filter(sex ==input$sex) %>%
                filter(bmi_label==input$bmi)%>%
                filter(children == input$children) %>%
                group_by(region) %>%
                summarise(avg_charges = mean(charges_annual)) %>%
                ggplot(mapping = aes(x=region, y= avg_charges, fill= region))+
                geom_col()
            
        )
        
    })
    
  
    output$md_19 = renderLeaflet({ 
        # For the legand box SCALE(bin) & Color and DOMAIN column
        bins <- c(0, 20, 40, 60, 80, 100, 125, 150, Inf)
        pal <- colorBin("YlOrRd", domain = mapd$C_19, bins = bins)
        # Displayed Box
        labels <- sprintf("<strong>%s</strong><br/>Discharged %g<br/>Submitted $%g<br/>Paid $%g<br/>Paid CMS $%g<br/>Density %g",
            mapd$state, mapd$D_19, mapd$C_19, mapd$P_19, mapd$MP_19, states$density) %>% lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 4) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(mapd$C_19),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            ###location of legand box #####input
            addLegend(pal = pal, values = ~mapd$C_19, opacity = 0.7, title = NULL, position = "bottomright")
    })
    
    output$md_18 = renderLeaflet({ 
        # For the legand box SCALE(bin) & Color and DOMAIN column
        bins <- c(0, 25, 50, 100, 150, 250, 500, 1000, Inf)
        pal <- colorBin("YlOrRd", domain = mapd$D_19, bins = bins)
        # Displayed Box
        labels <- sprintf("<strong>%s</strong><br/>Discharged %g<br/>Submitted $%g<br/>Paid $%g<br/>Paid CMS $%g<br/>Density %g",
            mapd$state, mapd$D_19, mapd$C_19, mapd$P_19, mapd$MP_19, states$density) %>% 
            lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 3.1) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(mapd$D_19),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    })
    
    output$md_17 = renderLeaflet({ 
        # # ORIGINAL For the legand box
        bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
        pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
        
        # Displayed Box
        labels <- sprintf("<strong>%s</strong><br/>Discharged %g<br/>Submitted $%g<br/>Paid $%g<br/>Paid CMS $%g<br/>Density %g",
            mapd$state, mapd$D_19, mapd$C_19, mapd$P_19, mapd$MP_19, states$density) %>% 
            lapply(htmltools::HTML)
        
        leaflet(states) %>%
            setView(-96, 37.8, 3.1) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
            addPolygons(
                fillColor = ~pal(states$density),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) 
    })


    
    
    ############### WORD CLOUD
    #input$range[1]
    output$wcoutput <- renderPlot ({
      wc <-   read_csv("wc19.csv")
      # group_by(word=word)%>%
      # summarise(freq=sum(freq))
      
      d <-   as.data.frame (wc,stringsAsFactors = FALSE) 
      frequency <- d$freq
      frequency=round(sqrt(d$freq),0) 
      frequency=sort(d$freq,decreasing = TRUE)
      
      set.seed(10)
      wordcloud(words = d$word, 
                freq = frequency,
                scale=c(3,.5), #######Q: scale min max frequencey
                min.freq = input$range[1],
                # max.freq =1100,#input$range[2],
                max.words = input$max,
                colors=brewer.pal(8, "Dark2"),
                random.order=FALSE)  
    }) 
    
    
    output$plot_NHE = renderPlot({
      
      nhe_ggp <- data.frame(x = nhe$Year,                            # Reshape data frame
                            y = c(nhe$Total, nhe$Insurance, nhe$Medicare, nhe$Medicaid),
                            Group = c(rep("Total", nrow(nhe)),
                                      rep("Insurance", nrow(nhe)),
                                      rep("Medicare", nrow(nhe)),
                                      rep("Medicaid", nrow(nhe))))
      
      return(
        ggplot(nhe_ggp, aes(x, y, col = Group)) +          
          geom_line(size = 1) +
          xlim(input$year[1],input$year[2])+
          labs(
            subtitle = "National Health Expenditure 1960 to 2030 (in millions)",
            x = "Year",
            y = "Expenditure") +
          theme_classic())
      
    })
    
}

shinyApp(ui = ui, server = server)

