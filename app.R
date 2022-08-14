library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(treemapify)
library(DT)


mypalette <- c('springgreen3','mediumseagreen','cornflowerblue','red','red','orange','tomato2','steelblue3','pink','grey',"goldenrod3","springgreen4","skyblue","orange3","blue3","darkgreen")
CC <- read_csv("CC.csv")
abc <- read_csv("am.csv")
N <- read_csv("nd.csv")
or <- read_csv("world3.csv")
energy1<- read_csv("energy5.csv")
l<- read_csv("globalenergy1.csv")
ui <- dashboardPage(title = "Climate change" , skin ="black",
                    dashboardHeader(title = "Climate Change"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "int"),
                        menuItem("Warning signs", tabName = "ws"),
                        menuItem("Result and Effect", tabName = "RE"),
                        menuItem("The Way Forward",tabName="wf"),
                        menuItem("Data Info",tabName = "dt")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        
                        tabItem(tabName = "int",
                                       
                                      fluidRow(column(12, box(width = 15,
                                           title = h3("Introduction"),
                                           status = "primary",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           height = "100%",span(textOutput("a"),style="font-size:18px")),
                                           box(width = 15,
                                               title = h3("Made by"),
                                               status = "primary",
                                               solidHeader = TRUE,
                                               collapsible = TRUE,
                                               collapsed = TRUE,
                                               height = "100%",span(textOutput("z"),style="font-size:18px"))
                                       ))),
                        
                        
                        
                        
                        
                        tabItem(tabName = "ws",
                               
                                  box(width = 15,
                                        title = ("Warning signs"),
                                        status = "primary",
                                        solidHeader = TRUE,
                                
                                        height = "100%",span(textOutput("b"),style="font-size:16px")
                                     ),
                                  fluidRow(
                                    box(selectInput("s", label = h3("Select Warning Sign"), choices = c("Sea Level","Antarctica mass","CO2 in Atmosphere"), selected = "SL"),
                                         box(plotOutput("signs"), width = 12)),
                                        
                                        box(width = 5,
                                            title = ("Chart Information"),
                                            status = "info",
                                            solidHeader = TRUE,
                                            collapsible = TRUE,
                                            collapsed = FALSE,
                                            height = "100%",face="bold", textOutput ("f"),style ="font-size:17px")
                                  ,
                              
                                 )
                               
                                
                        ),
                        
                            tabItem(tabName = "RE",
                                    box(width = 15,
                                        title = "Results of climate change",
                                        status = "primary",
                                        solidHeader = TRUE,
                                    
                                        height = "100%" ,span(textOutput("r"),style ="font-size:16px")),
                       
                                        fluidRow(
                                          
                                          box(plotOutput("ND"),width = 20,selectInput("a", label = "Select natural disaster", choices = c("All Stacked","Drought","Earthquake","Flood","Extreme Temperature","Extreme Weather","LandSlide","Volcanic Activity","Wildfire")))),
                                          fluidRow(
                                            box(plotOutput("results"),width = 20,sliderInput("year", "Year range :", min = 1880, max = 2016, value = c(1880, 2016))),width =20)
                                        
                                                                       
                                    ),
                        tabItem(tabName = "wf",
                                box(width = 20,
                                    title = "The Way Forward",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    
                                    height = "100%",span(textOutput("G"),style ="font-size:17px")),
                                box(width = 13,
                            
                                    height = "100%", 
                               fluidRow(column(12, plotOutput("W")
                                    ,
                                )
                                
                                
                                )),
                              fluidRow( box(width=20,plotOutput("l"),(plotOutput("e"))))
                               
                               
                               ),
                        tabItem(tabName = "dt",
                                fluidPage(column(12,fluidRow(box(width = 20,
                                    title = "Datasets and Sources",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    
                                    height = "100%"),
                               fluidRow( box(  selectInput("z1",label = h3("Select Dataset"), choices = c("Warning Signs","Total greenhouse gas emission by source","Greenhouse gas emissions(for 1 TWH) by source","Share of Greenhouse gas emission by company","Number of natural disasters","Mean Land-Ocean temp"), selected = "Mean Land-Ocean temp")),
                                  DT::dataTableOutput("mytable"),
                                box(title =  "Source links",collapsible = TRUE,collapsed = TRUE
                                    ,span(textOutput("sor"),style="font-size:14px")
                                    ,span(textOutput("sor1"),style="font-size:14px"),
                                    span(textOutput("sor2"),style="font-size:14px"),
                                    span(textOutput("sor3"),style="font-size:14px"),
                                    span(textOutput("sor4"),style="font-size:14px"),
                                    span(textOutput("sor5"),style="font-size:14px")
                                    )
                            
                               )
                                
                                
                                ))

                                
                                
                                
                                
                                ))
                        
                        
                                
                        
                                    
                                    

                                    
                                    
                                    
                      
                    ) 
)
)

server <- function(input, output) { 
  
  output$signs <- renderPlot({
  
      if(input$s == "Sea Level"){
        CC %>%
          ggplot(aes(x = Year )) + 
          geom_line(aes(y= SL),color= "blue2",size=1) +
          ggtitle("Change in Sea Level(mm)")+
          theme_minimal()+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"),axis.title.x = element_text(color="Black", size=14, face="bold"),axis.title.y = element_blank())

                                  
                                
        
        
      } else if(input$s == "Antarctica mass"){
        CC %>%
          ggplot(aes(x = Year )) + 
          geom_line(aes(y= AM ),color = "skyblue",size=1) +
          ggtitle("Change in Antarcticas Mass(Gigatonnes)"
                  )+
          theme_minimal()+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"),axis.title.x = element_text(color="Black", size=14, face="bold"),axis.title.y = element_blank())
        
  

        
        
      }  else if(input$s == "CO2 in Atmosphere"){
        CC %>%
          ggplot(aes(x = Year )) + 
          geom_line(aes(y= CO2 ),color = "red",size=1) +
          ggtitle("CO2 particles per million Air particles")+
          theme_minimal()+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"),axis.title.x = element_text(color="Black", size=14, face="bold"),axis.title.y = element_blank())
        
  
        
      }
  })

  output$results <- renderPlot({
   abc%>%
      ggplot(aes(x = Year))+coord_cartesian(xlim=input$year)+
      geom_bar(aes(y= Mean ),stat="identity",fill = ifelse(abc$Mean > 0, "firebrick4", "dodgerblue3")) + 
      ggtitle("Mean Surface-Ocean Tempearature")+
      theme_minimal()+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
  })
  output$ND <- renderPlot({
    
    if(input$a == "Drought"){
      N%>%
        filter(Entity == "Drought")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity",fill="sandybrown")+
        ggtitle("Number of droughts over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
    }
    else if(input$a == "Wildfire"){
      N%>%
        filter(Entity == "Wildfire")%>%
          ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
          geom_bar(position="stack", stat="identity",fill="tomato1")+
        ggtitle("Number of Wildfires over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
      }
    else if(input$a == "All Stacked"){
      N%>%
        filter(Entity !="All natural disasters")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity")+ scale_fill_manual(values=c("sandybrown",
                                                                                "wheat3",
                                                                                "red2",
                                                                                "darkmagenta","royalblue","goldenrod4","firebrick4","tomato1"))+
        ggtitle("Number of natural disasters over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
      
      }
    else if(input$a == "LandSlide"){
      N%>%
      filter(Entity == "Landslide")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity",fill="goldenrod4") +ggtitle("Number of landslides over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))

      }
    else if(input$a == "Extreme Weather"){
      N%>%
      filter(Entity == "Extreme weather")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity", fill = "darkmagenta")+ ggtitle("Number of cases of extreme weather over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
}
    else if(input$a == "Extreme Temperature"){
      N%>%
      filter(Entity == "Extreme temperature")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity",fill="red2") + ggtitle("Number of cases of extreme temprature over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
}
    else if(input$a == "Earthquake"){
      N%>%
      filter(Entity == "Earthquake")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity",fill="wheat3") + ggtitle("Number of Earthquakes over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
}
    else if(input$a == "Flood"){
      N%>%
        
      filter(Entity == "Flood")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity",fill="royalblue") + ggtitle("Number of floods over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
}
    else if(input$a == "Volcanic Activity"){
      N%>%
      filter(Entity == "Volcanic activity")%>%
        ggplot( aes(fill=Entity, y=Nfd, x=Year)) + 
        geom_bar(position="stack", stat="identity",fill="firebrick4") +ggtitle("Number of instances of volcanic activity over the years")+theme(plot.title = element_text(hjust = 1, size = 16, face = "bold"))
}
    
  })
  
  output$f <- renderText({
    if(input$s == "CO2 in Atmosphere"){("The line chart shows how global carbon dioxide has changed over time ,Carbon dioxide (CO2) is an important heat-trapping gas, or greenhouse gas, that comes from the extraction and burning of fossil fuels (such as coal, oil, and natural gas), from wildfires, and from natural processes like volcanic eruptions")}

  
  
  else if(input$s == "Sea Level"){("The line chart shows the rise in Sea level over the years,Sea level rise is caused primarily by two factors related to global warming: the added water from melting ice sheets and glaciers, and the expansion of seawater as it warms")}
  
  
  
  else  if(input$s == "Antarctica mass"){("This line chart shows the loss of Antarcticas mass over the years,This is important because the ice sheets of Greenland and Antarctica store about two-thirds of all the fresh water on Earth. They are losing ice due to the ongoing warming of Earth's surface and ocean. Meltwater coming from these ice sheets is responsible for about one-third of the global average rise in sea level since 1993")}
  })
  output$b <- renderText({
    ("The chart below maps the progress of the 3 main measures used to track climate change")
    
    
  })
  output$r <- renderText({
    ("The charts below highlight the two main effects of climate change,rising temperature and an increase in natural disasters") 
    
    
  })
  output$W<- renderPlot({
       or %>%
      ggplot(aes(area = GHG , fill= Company,label = paste(Company, GHG, sep = "\n"))) +
      geom_treemap() + ggtitle("Worldwide Greenhouse Gas emissions(%)",subtitle = "Energy companies vs The World")+ theme(plot.title = element_text(size = 22, face = "bold"))+
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15) +
      theme(legend.position = "none")+scale_fill_manual(values = mypalette)

      
      
    
  }) 
  output$G <- renderText({"70% of the Worlds greenhouse gas emissions are from energy companies,the way forward is to change the source of energy produced"})
                         
  output$e<- renderPlot({
    energy1%>%
      ggplot(aes(x = Source,fill= Source )) +   
      geom_bar(aes(y= GHG,),stat="identity") +

      scale_fill_manual(values=c("saddlebrown","dimgrey","royalblue4","dimgrey","cornflowerblue","firebrick2","black","darkorange","skyblue"))+
      ggtitle("Greenhouse gas emission per TWH produced") +theme_void()+theme(plot.title = element_text(size = 15, face = "bold"))+
      coord_flip()
    
    
    
    
  })
  output$l<-renderPlot({
    l%>%
    ggplot( aes(x="", y=TWH, fill=Source)) +
      geom_bar(stat="identity", width=1) +ggtitle("Global Energy Consumption Share by Source")+ theme_void()+
                                                                                                      
      theme(plot.title = element_text(size = 15, face = "bold"))+
      
      coord_polar("y", start=0)+scale_fill_manual(values=c("saddlebrown","dimgrey","royalblue4","cornflowerblue","firebrick2","black","darkorange","skyblue"))
    
    
    
    
  })
  output$a<-renderText("The goal of this application is to show the warning signs used to track climate change, the results of climate change and what has to be done to tackle it")
  output$z <- renderText("This application was made by Rithvik Reddy,Currently a student at Johns Hopkins Carey Buisness school pursuing a degree in Buisness analytics and financial risk management.The application was made for a project in the Data Visualization Course")
  
  output$mytable <- DT::renderDataTable({
    if(input$z1 == "Warning Signs"){
      datatable(CC)
    }
    else if(input$z1 == "Total greenhouse gas emission by source"){
      datatable(l)
    }   
    else if(input$z1 == "Greenhouse gas emissions(for 1 TWH) by source"){
      datatable(energy1)
    }   
    else if(input$z1 == "Share of Greenhouse gas emission by company"){
      datatable(or)
    }  
    else if(input$z1 == "Mean Land-Ocean temp"){
      datatable(abc)
      
    } 
    else if(input$z1 == "Number of natural disasters"){
      datatable(N)
    }  
  }) 
  output$sor<- renderText("https://urs.earthdata.nasa.gov/profile")
  output$sor1<- renderText("https://cdn.cdp.net/cdp-production/cms/reports/documents/000/002/327/original/Carbon-Majors-Report-2017.pdf?1501833772")
  output$sor2<- renderText("http://www.uni-obuda.hu/users/grollerg/LCA/hazidolgozathoz/lca-electricity%20generation%20technologies.pdf")
  output$sor3<- renderText("https://ourworldindata.org/energy-key-charts")
  output$sor4<- renderText("https://ourworldindata.org/grapher/number-of-natural-disaster-events")
  output$sor5<- renderText("  http://berkeleyearth.org/global-temperature-report-for-2021")
  
  
  
  
  
}

shinyApp(ui, server)
