#Check for installed packages and install them if they're not installed:
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}
#Require packages:
required.packages <- c("shiny",
                       "shinydashboard",
                       "shinythemes",
                       "readxl",
                       "dplyr",
                       "tidyr",
                       "ggplot2",
                       "scales",
                       "DT",
                       "broom")
install(required.packages)

##############################################################

CNGRSS <- read_excel("CNGRSS.xlsx")

#View(CNGRSS)
cdf <- data.frame(CNGRSS)
#head(cdf)

## gathering all categories with year
cdf2 <- cdf %>%
  select(HouseDemocrats,SenateDemocrats,TotalDemocrats,
         HouseRepublicans,SenateRepublicans,TotalRepublicans,
         HouseOther,SenateOther,TotalOther,TotalMembers,
         Female,Male,Year)
long_cdf <- gather(cdf2, Categories, Amounts, 1:12)

###########################################################################################      
########################################################################################### 
shinyApp(
  ui = tagList(
    navbarPage(
      theme = shinythemes::shinytheme("sandstone"),
      "U.S Congress",
      ###########################################################################################      
      ########################################################################################### 
      tabPanel("1917-2020",
               sidebarPanel(
                 selectInput(inputId = "Category1",
                             label="Select First Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="Female"),
                 selectInput(inputId = "Category2",
                             label="Select Second Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="Male"),
                 selectInput(inputId = "Category3",
                             label="Select Third Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="Choose None")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Line Plot",
                            plotOutput("comparison")
                   ),
                   tabPanel("Scatter Plot", "Click on a point to get associated values:",
                            plotOutput("scatter", click = "plot_click"),
                            verbatimTextOutput("info")
                   ),
                   tabPanel("Box Plot",
                            plotOutput("box")
                   )
                 )
               )
      ),
      ###########################################################################################      
      ########################################################################################### 
      tabPanel("Select Year",
               sidebarPanel(
                 sliderInput(inputId = "Year",
                             label="Years of Congress from 1917-2020",
                             min = 1917,
                             max = 2020,
                             value = 1989),
                 selectInput(inputId = "Categ1",
                             label="Select First Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="TotalDemocrats"),
                 selectInput(inputId = "Categ2",
                             label="Select Second Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="TotalRepublicans"),
                 selectInput(inputId = "Categ3",
                             label="Select Third Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="TotalOther")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Pie Chart",
                            plotOutput("pieplot")
                   )
                 )
               )
      ),
      ###########################################################################################      
      ###########################################################################################       
      tabPanel("Regression Test",
               sidebarPanel(
                 selectInput(inputId = "Cat1",
                             label="Select a Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="Female")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Testing if significant relatinship between years and chosen category.",
                            verbatimTextOutput("reg")
                   ),
                   tabPanel("Prediction",
                            verbatimTextOutput("pre")
                   )
                 )
               )
      ),
      ###########################################################################################      
      ###########################################################################################       
      tabPanel("2-Sided T-Test",
               sidebarPanel(
                 selectInput(inputId = "Cate1",
                             label="Select First Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="HouseDemocrats"),
                 selectInput(inputId = "Cate2",
                             label="Select Second Category",
                             choices=c("Choose None","Female","Male","HouseDemocrats",
                                       "HouseRepublicans","HouseOther","SenateDemocrats",
                                       "SenateRepublicans","SenateOther","TotalDemocrats","TotalRepublicans",
                                       "TotalOther","TotalMembers" ),
                             selected="HouseRepublicans")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Testing if averages of 2 chosen categories are significantly different:",
                            verbatimTextOutput("tTest")
                   )
                 )
               )
      ),
      ###########################################################################################      
      ###########################################################################################      
      tabPanel('Data',     DT::dataTableOutput('ex1'))
    )
  ),
  ###########################################################################################      
  ########################################################################################### 
  server = function(input, output) {
    output$comparison <- renderPlot({
      data_cat<-subset(long_cdf,Categories==c(input$Category1,
                                              input$Category2,
                                              input$Category3))
      ggplot(data_cat, aes(x=Year,
                           y=Amounts,
                           group=Categories,
                           colour=Categories)) +
        geom_line()
    })
    ###########################################################################################      
    ###########################################################################################    
    output$scatter <- renderPlot({
      data_cat<-subset(long_cdf,Categories==c(input$Category1,
                                              input$Category2,
                                              input$Category3))
      ggplot(data_cat, aes(x=Year,
                           y=Amounts,
                           color=Categories,
                           shape=Categories)) +
        geom_point() + 
        geom_smooth(method=lm, aes(fill=Categories))
    })
    ###########################################################################################      
    ###########################################################################################  
    output$info <- renderText({
      paste0("Approximate Year:", input$plot_click$x, "\nAproximate Amount:", input$plot_click$y)
    })
    ###########################################################################################      
    ###########################################################################################     
    output$box <- renderPlot({
      data_cat<-subset(long_cdf,Categories==c(input$Category1,
                                              input$Category2,
                                              input$Category3))
      means <- aggregate(Amounts ~  Categories, data_cat, mean)
      ggplot(data_cat, aes(x=Categories,
                           y=Amounts,
                           colour=Categories)) +
        geom_boxplot()+
        stat_summary(fun.y=mean, colour="yellow", geom="point", 
                     shape=18, size=3,show_guide = FALSE) + 
        geom_text(data = means, color="black",aes(label = Amounts, y = Amounts + 0.08))
    })
    ###########################################################################################      
    ###########################################################################################     
    output$pieplot <- renderPlot({
      yr <- long_cdf %>%
        filter(Year == input$Year)
      cat1 <- yr %>%
        filter(Categories == input$Categ1)
      cat2 <- yr %>%
        filter(Categories == input$Categ2)
      cat3 <- yr %>%
        filter(Categories == input$Categ3)
      catDf <- cat1 %>%
        bind_rows(cat2,cat3)
      pieDf <- catDf %>%
        select(Categories, Amounts)
      boxP<- ggplot(pieDf, aes(x="", y=Amounts, fill=Categories))+
        geom_bar(width = 1,stat = "identity")+
        geom_label(aes(label = percent(Amounts/sum(Amounts))), size=5)
      boxP
      pieC <- boxP + coord_polar("y", start=0)
      pieC
    }) 
    ###########################################################################################      
    ###########################################################################################
    output$reg <- renderPrint({
      regDf <- long_cdf %>%
        filter(Categories == input$Cat1) %>%
        select(Year, Amounts)
      catLm <- lm(formula = Year ~ Amounts, data = regDf)
      catLmGlance <- glance(catLm)
      if (catLmGlance$p.value < 0.05){
        paste("The resulting P-value", catLmGlance$p.value,
              "is less than 0.05 level of significance.", 
              "Thus, we reject the null hypothesis,",
              "and conclude there is a significant relationship",
              "between the years 1917-2020 and the chosen category.")
      } else if (catLmGlance$p.value > 0.05){
        paste("The resulting P-value ", catLmGlance$p.value, 
              "is greater than 0.05 level of significance.",
              "Thus, we fail to reject the null hypothesis,",
              "and do not have enough statistical evidence to claim",
              "that there is a significant relationship",
              "between the years 1917-2020 and the chosen category")
      }
    })
    ###########################################################################################      
    ###########################################################################################
    output$pre <- renderPrint({
      regDf <- long_cdf %>%
        filter(Categories == input$Cat1) %>%
        select(Year, Amounts)
      catLm <- lm(formula = Year ~ Amounts, data = regDf)
      catLmsumCoefDf <- summary(catLm)$coefficients
      catLmsumCoefDfPval <- catLmsumCoefDf[2,4]
      catLmsumCoefDfPval
      catLmsumCoefDfSlope <- catLmsumCoefDf[2,1]
      catLmsumCoefDfSlope
      if (catLmsumCoefDfPval<0.05 & catLmsumCoefDfSlope>0){
        paste("As years increase, amount of chosen category increases.")
      } else if (catLmsumCoefDfPval<0.05 & catLmsumCoefDfSlope<0){
        paste("As years increase, amount of chosen category decreases.")
      } else if (catLmsumCoefDfPval>0.05){
        paste("Inconclusive.")
      }
    })
    ###########################################################################################      
    ###########################################################################################
    output$tTest <- renderPrint({
      tTestDf <- long_cdf %>%
        filter(Categories == input$Cate1 | Categories == input$Cate2) %>%
        select(Categories, Amounts)
      #test for normality:
      nRes1 <- with(tTestDf, shapiro.test(Amounts[Categories == input$Cate1]))
      nRes1Glance <- glance(nRes1)
      nRes1Glance$p.value
      nRes2 <- with(tTestDf, shapiro.test(Amounts[Categories == input$Cate2]))
      nRes2Glance <- glance(nRes2)
      nRes2Glance$p.value
      #test for equal variances:
      vRes <- var.test(Amounts ~ Categories, data = tTestDf)
      vResGlance <- glance(vRes)
      vResGlance$p.value
      #2-sided t-test:
      catTtest <- t.test(Amounts ~ Categories, data = tTestDf, var.equal = TRUE)
      catTtestGlance <- glance(catTtest)
      #function to run if assumptions true:
      assumpFunct <- (if (catTtestGlance$p.value < 0.05){
        paste("The resulting P-value", catTtestGlance$p.value,
              "is less than 0.05 level of significance.", 
              "Thus, we reject the null hypothesis,",
              "and conclude there is a significant difference",
              "between the averages of the two chosen categories.")
      } else if (catTtestGlance$p.value > 0.05){
        paste("The resulting P-value ", catTtestGlance$p.value, 
              "is greater than 0.05 level of significance.",
              "Thus, we fail to reject the null hypothesis,",
              "and do not have enough statistical evidence to claim",
              "that there is a significant difference",
              "between the averages of the two chosen categories.")
      })
      #testing:
      if (nRes1Glance$p.value>0.05 & nRes2Glance$p.value>0.05 & vResGlance$p.value>0.05){
        assumpFunct
      } else
        paste("One or more assumptions required for performing this test failed.")
    })
    ###########################################################################################      
    ###########################################################################################
    output$ex1 <- DT::renderDataTable(
      DT::datatable(cdf, options = list(pageLength = 10))
    )
    ###########################################################################################      
    ########################################################################################### 
  }
)
shinyApp(ui = ui, server = server)
