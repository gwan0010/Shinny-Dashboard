# import libraries
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(e1071)
library(neuralnet)
library(corrplot)
library(car)
library(e1071)
library(neuralnet)
library(rpart)
library(randomForest)
library(DT)
library(plotly)
library(cluster)
library(fpc)
library(wordcloud2)
library(caTools)



# read data
# please specify the file directory
df_Choropleth <- read.csv('happyWithCountryCode.csv')
cluster_country_combine <- read.csv('cluster_country_combine.csv')
df_stati <- read.csv('CountryRegionCombine.csv')

#########################################################################################

# dropdown menu inputs
region_names <- df_stati$Region%>%unique()
year <- df_stati$Year%>%unique()
prediction_algorithms <- list('Multiple Linear Regression','Support Vector Regression',
                              'Decision Tree Regression','Random Forest Regression')

#########################################################################################

#machine learning plots
#data prepare for machine learning
df_predict <- df_stati%>%select(-c(Region,Country,Year))
df_cluster <- df_stati%>%select(-c(Region,Country,Happiness.Score))
df_cluster_2015 <- df_cluster[df_cluster$Year == 2015,]


split = sample.split(df_predict$Happiness.Score, SplitRatio = 0.8)
training_set = subset(df_predict, split == TRUE) 
test_set = subset(df_predict, split == FALSE)

# lr
regressor_lm = lm(formula = Happiness.Score ~ .,data = training_set)
y_pred_lm = predict(regressor_lm, newdata = test_set)
Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Happiness.Score))
summary.lm <- summary(regressor_lm)
gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
# lr summary to print
summary.lm <- summary(regressor_lm)


# regression tree
regressor_dt = rpart(formula = Happiness.Score ~ .,
                     data = training_set,
                     control = rpart.control(minsplit = 10))

y_pred_dt = predict(regressor_dt, newdata = test_set)

Pred_Actual_dt <- as.data.frame(cbind(Prediction = y_pred_dt, Actual = test_set$Happiness.Score))


gg.dt <- ggplot(Pred_Actual_dt, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

# decision tree summary to print
summary.dt <- summary(regressor_dt)

# rf
regressor_rf = randomForest(x = training_set%>%select(-c(Happiness.Score)),
                            y = training_set$Happiness.Score,
                            ntree = 500)

y_pred_rf = predict(regressor_rf, newdata = test_set)

Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Happiness.Score))


gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

# random froest summary to print
summary.rf <- summary(regressor_rf)

#SVR

regressor_svr = svm(formula = Happiness.Score ~ .,
                    data = training_set,
                    type = 'eps-regression',
                    kernel = 'radial')

y_pred_svr = predict(regressor_svr,  newdata = test_set)

Pred_Actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$Happiness.Score))


Pred_Actual_lm.versus.svr <- cbind(Prediction.lm = y_pred_lm, Prediction.svr = y_pred_svr, Actual = test_set$Happiness.Score)


gg.svr <- ggplot(Pred_Actual_svr, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))

# support vector regression summary to print
summary.svr <- summary(regressor_svr)

#########################################################################################


#word Cloud dataprepare
df_cor <- df_stati%>% select(c("Dystopia.Residual",'GDP.per.Capita',"Family","Freedom","Generosity",
                         "Life.Expectancy","Government.Corruption",'Happiness.Score','Year'))


#wrangling function to ouput the correlation dataframe
wrang_dataframe <- function(input_DataFrame) {
  
  mydata.cor <- cor(input_DataFrame$Happiness.Score,input_DataFrame)
  baskets.df <- as.data.frame(t(mydata.cor))
  baskets.df$name <- rownames(baskets.df)
  baskets.df <- baskets.df[0:7,]
  baskets.df <- baskets.df[c(2,1)]
  
  return(baskets.df)
}

df_cor_2015 <- wrang_dataframe(df_cor[df_cor$Year == 2015,])
df_cor_2016 <- wrang_dataframe(df_cor[df_cor$Year == 2016,])
df_cor_2017 <- wrang_dataframe(df_cor[df_cor$Year == 2017,])

#########################################################################################


#shiny dashboard

ui <- dashboardPage(
  dashboardHeader(title = 'World Happiness Report'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('World Happiness Distribution',tabName = 'first'),
      menuItem('Regional Statistical Distribution',tabName = 'third'),
      menuItem('Attributes affect Happiness',tabName = 'second'),
      menuItem('Calculation',tabName = 'fourth'),
      menuItem('Clustering Countries', tabName = 'fifth')
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'first', h2('Geographical Distribution of the Happiness Score'),
              fluidRow(
                box(selectInput(inputId = 'select_year_tabOne',
                                label = 'Choose Year',
                                choices = list(2015,2016,2017)),width = 3),
                valueBoxOutput('happiest'),
                valueBoxOutput('sadest')
                
              ),

              fluidRow(
                plotlyOutput('world_map'),width = '20'
              )
      ),
      
      
      tabItem(tabName = 'second', h2('Attributes affect Happiness'),
              fluidRow(
                # box(leafletOutput('plot2',height = 400,width = 400)),
                box(selectInput(inputId = 'select_year_tabTwo',
                                label = 'Choose Year',
                                choices = year)),
                box(selectInput(inputId = 'choose_column_tabTwo',
                                label = 'Choose Attribute',
                                choices = list("Dystopia.Residual",'GDP.per.Capita',"Family","Freedom","Generosity",
                                               "Life.Expectancy","Government.Corruption"),
                                multiple = TRUE,
                                selected = 'GDP.per.Capita'
                ))
                
              ),
              
              
              fluidRow(
                box(wordcloud2Output("word_cloud")),
                box(plotOutput('correlation',height = 500,width = 500))
              )),
      
      tabItem(tabName = 'third', h2('Statistical Distribution in Regions'),
              fluidRow(
                box(selectInput(inputId = 'select_year_tabThree',
                                label = 'Choose Year',
                                choices = year),width = 4),
                valueBoxOutput('happiest_region',width = 4),
                valueBoxOutput('saddest_region',width = 4)
              ),
              fluidRow(
                column(6,
                       plotOutput("plot1", click = "plot1_click")
                ),
                column(6,
                       plotOutput('plot2', click = 'dt_click')
                )
              )),
      
      
      tabItem(tabName = 'fourth', h2('Machine Learning to Predict Happiness Score'),
              fluidRow(
                valueBoxOutput('best_algorithm',width = 6),
                box(selectInput(inputId = 'choose_method_tabFour',
                                label = 'Choose Machine Learning Algorithm',
                                choices = prediction_algorithms))
                
              ),
              fluidRow(
                box(plotOutput('predict'),width = 6),
                box(title = "Algorithm Summary", width = 6, status = "primary",
                    verbatimTextOutput('algorithm_summary'))
              )),
      
      tabItem(tabName = 'fifth', h2('Clustering Algorithms to Group Countries'),
              fluidRow(
                box(selectInput(inputId = 'select_year_tabFive',
                                label = 'Choose Year',
                                choices = year),width = 4),
                
                box(selectInput(inputId = 'select_clustertabFive',
                                label = 'Choose Cluster Method',
                                choices = list('Kmean','Hieratical')),width = 4),
                box(selectInput(inputId = 'select_leveltabFive',
                                label = 'Choose Level',
                                choices = list(1,2,3,4,5)),width = 4)
              ),
              fluidRow(
                box(plotlyOutput('clustering_map')),
                box(dataTableOutput('clustering_country_dt'))
              ))
      
      
      
    )
  )
)




server <- function(input, output) {
  
  #tab one data prepare
  df_Choropleth_input <- reactive({
    df_Choropleth[df_Choropleth$Year == input$select_year_tabOne,]
  })
  
  #tab one value box
  output$happiest <- renderValueBox({
    
    selected_df <- df_Choropleth[df_Choropleth$Year == input$select_year_tabOne,]
    happiest_country <- selected_df[selected_df$Happiness.Score == selected_df$Happiness.Score%>%max, "Country"]
    
    valueBox(
      happiest_country, "Happiest Country", icon = icon("smile"),
      color = "green",width = 4
    )
  })
  
  output$sadest <- renderValueBox({
    
    selected_df <- df_Choropleth[df_Choropleth$Year == input$select_year_tabOne,]
    sad_country <- selected_df[selected_df$Happiness.Score == selected_df$Happiness.Score%>%min, "Country"]
    
    valueBox(
      sad_country, "Saddest Country", icon = icon("sad-tear"),
      color = "red",width = 4
    )
  })
  
  
  
  #tab one map
  output$world_map <- renderPlotly({
    
    l <- list(color = toRGB("Grey"), width = 1)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df_Choropleth_input()) %>%
      add_trace(
        z = ~Happiness.Score, color = ~Happiness.Score, colors = 'Blues',
        text = ~Country, locations = ~CODE, marker = list(line = l)
      ) %>%
      layout(autosize = FALSE, width = 1508, height = 700) %>%
      colorbar(title = 'Happiness Score') %>%
      layout(
        geo = g
      )
  })
  
  

  
  #tab two
  #data prepare
  # interactive value box
  output$happiest_region <- renderValueBox({
    
    if (is.null(input$plot1_click$y)) {a = df_stati[(df_stati$Year == input$select_year_tabThree),] }
    else {
      
      if (input$plot1_click$y >0.5 & input$plot1_click$y < 1.5) {keeprow = 'Australia and New Zealand'}
      if (input$plot1_click$y >1.5 & input$plot1_click$y < 2.5) {keeprow = 'Central and Eastern Europe'}
      if (input$plot1_click$y >2.5 & input$plot1_click$y < 3.5) {keeprow = 'Eastern Asia'}
      if (input$plot1_click$y >3.5 & input$plot1_click$y < 4.5) {keeprow = 'Latin America and Caribbean'}
      if (input$plot1_click$y >4.5 & input$plot1_click$y < 5.5) {keeprow = 'Middle East and Northern Africa'}
      if (input$plot1_click$y >5.5 & input$plot1_click$y < 6.5) {keeprow = 'North America'}
      if (input$plot1_click$y >6.5 & input$plot1_click$y < 7.5) {keeprow = 'Southeastern Asia'}
      if (input$plot1_click$y >7.5 & input$plot1_click$y < 8.5) {keeprow = 'Southern Asia'}
      if (input$plot1_click$y >8.5 & input$plot1_click$y < 9.5) {keeprow = 'Sub-Saharan Africa'}
      if (input$plot1_click$y >9.5 & input$plot1_click$y < 10.5) {keeprow = 'Western Europe'}
      
      a = df_stati[(df_stati$Year == input$select_year_tabThree & df_stati$Region == keeprow),]
    }
    
    region_happiest <- a[a$Happiness.Score == a$Happiness.Score%>%max(), "Country"]
    
    valueBox(
      region_happiest, "Happiest Country", icon = icon("smile"),
      color = "green",width = 4
    )
  })
  
  #interactive value box
  output$saddest_region <- renderValueBox({
    
    if (is.null(input$plot1_click$y)) {a = df_stati[(df_stati$Year == input$select_year_tabThree),] }
    else {
      
      if (input$plot1_click$y >0.5 & input$plot1_click$y < 1.5) {keeprow = 'Australia and New Zealand'}
      if (input$plot1_click$y >1.5 & input$plot1_click$y < 2.5) {keeprow = 'Central and Eastern Europe'}
      if (input$plot1_click$y >2.5 & input$plot1_click$y < 3.5) {keeprow = 'Eastern Asia'}
      if (input$plot1_click$y >3.5 & input$plot1_click$y < 4.5) {keeprow = 'Latin America and Caribbean'}
      if (input$plot1_click$y >4.5 & input$plot1_click$y < 5.5) {keeprow = 'Middle East and Northern Africa'}
      if (input$plot1_click$y >5.5 & input$plot1_click$y < 6.5) {keeprow = 'North America'}
      if (input$plot1_click$y >6.5 & input$plot1_click$y < 7.5) {keeprow = 'Southeastern Asia'}
      if (input$plot1_click$y >7.5 & input$plot1_click$y < 8.5) {keeprow = 'Southern Asia'}
      if (input$plot1_click$y >8.5 & input$plot1_click$y < 9.5) {keeprow = 'Sub-Saharan Africa'}
      if (input$plot1_click$y >9.5 & input$plot1_click$y < 10.5) {keeprow = 'Western Europe'}
      
      a = df_stati[(df_stati$Year == input$select_year_tabThree & df_stati$Region == keeprow),]
    }
    
    region_sad <- a[a$Happiness.Score == a$Happiness.Score%>%min(), "Country"]
    
    valueBox(
      region_sad, "Saddest Country", icon = icon("sad-tear"),
      color = "red",width = 4
    )
  })
  
  
  #the boxplot
  output$plot1 <- renderPlot({
    
    input_tab2 <- df_stati[df_stati$Year == input$select_year_tabThree,]
    
    gg2 <- ggplot(input_tab2 , aes(x = input_tab2$Region, y = input_tab2$Happiness.Score)) +
      geom_boxplot(aes(fill=input_tab2$Region)) + theme_bw() +
      theme(axis.title = element_text(family = "Helvetica", size = (6)))
    
    gg2 <- gg2+coord_flip()+ xlab('Region Continent')+ylab('Happiness Score')
    gg2
  })
  
  # narrarive histogram
  output$plot2 <- renderPlot({
    
    input_tab2 <- df_stati[df_stati$Year == input$select_year_tabThree,]
    
    if (is.null(input$plot1_click$y)) return(hist(input_tab2$Happiness.Score, 
                                                  xlab = 'Happiness Score' , 
                                                  main = 'Histogram', col='light blue'))
    else {
      
      if (input$plot1_click$y >0.5 & input$plot1_click$y < 1.5) {keeprow = 'Australia and New Zealand'}
      if (input$plot1_click$y >1.5 & input$plot1_click$y < 2.5) {keeprow = 'Central and Eastern Europe'}
      if (input$plot1_click$y >2.5 & input$plot1_click$y < 3.5) {keeprow = 'Eastern Asia'}
      if (input$plot1_click$y >3.5 & input$plot1_click$y < 4.5) {keeprow = 'Latin America and Caribbean'}
      if (input$plot1_click$y >4.5 & input$plot1_click$y < 5.5) {keeprow = 'Middle East and Northern Africa'}
      if (input$plot1_click$y >5.5 & input$plot1_click$y < 6.5) {keeprow = 'North America'}
      if (input$plot1_click$y >6.5 & input$plot1_click$y < 7.5) {keeprow = 'Southeastern Asia'}
      if (input$plot1_click$y >7.5 & input$plot1_click$y < 8.5) {keeprow = 'Southern Asia'}
      if (input$plot1_click$y >8.5 & input$plot1_click$y < 9.5) {keeprow = 'Sub-Saharan Africa'}
      if (input$plot1_click$y >9.5 & input$plot1_click$y < 10.5) {keeprow = 'Western Europe'}
      
      
      df_boxplot_hist <- input_tab2[input_tab2$Region == keeprow,]
      hist(df_boxplot_hist$Happiness.Score, 
           xlab = 'Happiness Score' , 
           main = 'Histogram', col='light blue')
    }
  })
  

  
  # tab three 
  # reactive input
  plot2_data_input <- reactive({
    df_stati[df_stati$Year==input$select_year_tabTwo,]%>%select(input$choose_column_tabTwo,Happiness.Score)
  })
  
  # correlation 
  output$correlation <- renderPlot({
    corrplot(cor(plot2_data_input()), method="circle", type='lower', tl.cex=0.9, tl.col = 'black',addCoef.col = "black")
  })
  
  # #world_cloud
  output$word_cloud <- renderWordcloud2({
    
    df_cor_input <- wrang_dataframe(df_cor[df_cor$Year == input$select_year_tabTwo,])
    
    wordcloud2(df_cor_input, size = 0.4,color = "random-light", backgroundColor = "white",widgetsize = 1000,
               shape = "'diamond'")
  })
  
  
  #tab4
  #plot for different between actual and learning 
  output$best_algorithm <- renderValueBox({
    
    valueBox(
      'Multiple Linear Regression', "Best Algorithm", icon = icon("thumbs-up"),
      color = "green", width = NULL
    )})
  
  output$predict <- renderPlot({
    if ( input$choose_method_tabFour =='Multiple Linear Regression') print(gg.lm)
    if ( input$choose_method_tabFour =='Support Vector Regression') print(gg.svr)
    if ( input$choose_method_tabFour =='Decision Tree Regression') print(gg.dt)
    if ( input$choose_method_tabFour =='Random Forest Regression') print(gg.rf)
    if ( input$choose_method_tabFour =='Neural Net') print(gg.nn)
  })
  
  #plot for algorithm summary
  output$algorithm_summary <- renderPrint({
    if ( input$choose_method_tabFour =='Multiple Linear Regression') print(summary.lm)
    if ( input$choose_method_tabFour =='Support Vector Regression') print(summary.svr)
    if ( input$choose_method_tabFour =='Decision Tree Regression') print(summary.dt)
    if ( input$choose_method_tabFour =='Random Forest Regression') print(summary.rf)
  })
  
  
  #tab5
  df_cluster_input <- reactive({
    cluster_country_combine[(cluster_country_combine$Year == input$select_year_tabFive &
                               cluster_country_combine$Method == input$select_clustertabFive),]
  })
  
  # tab five map
  output$clustering_map <- renderPlotly({
    
    l <- list(color = toRGB("Grey"), width = 1)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df_cluster_input()) %>%
      add_trace(
        z = ~Value, color = ~Value, colors = 'Blues',
        text = ~Country, locations = ~code, marker = list(line = l)
      ) %>%
      layout(autosize = TRUE) %>%
      colorbar(title = 'Happiness Score') %>%
      layout(
        title = 'Global Happiness Score',
        geo = g
        
      )
  })
  
  #data table
  output$clustering_country_dt <- renderDataTable({
    data <- cluster_country_combine
    data <- data[(data$Year == input$select_year_tabFive &
                    data$Method == input$select_clustertabFive &
                    data$Value == input$select_leveltabFive),]%>%select(-c(code))
    data
    
  })
  
}

shinyApp(ui, server)


