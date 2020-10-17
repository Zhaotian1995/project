#install.packages("shinydashboard")
library(ggplot2)
library(mice) 
library(lattice)
library(caret)
library(readxl)
library(corrplot)
library(shinydashboard)
library(shiny)
library(psych)
library(pastecs)
library(shiny)
library(quantmod)
library(shinythemes)
library(plotly)
library(PortfolioAnalytics)
library(quantmod)
library(foreach)
library(DEoptim)
library(iterators)
library(fGarch)
library(lubridate)
library(quadprog)
library(quantmod)
library(pso)
library(shinycssloaders)
library(GenSA)
library(corpcor)
library(testthat)
library(nloptr)
library(MASS)
library(robustbase)
library(tidyquant)
library(timetk)
library(dplyr)
require(zoo)
require(tseries)
library(forecast)
library(xts)
library(h2o)
require(dplyr)
require(highcharter) #to plot amazing time series plots
library(readxl)
library(earth)
require(earth)
require(tidyr)
library(dplyr)
library(tidyverse)
library(tibbletime)
library(h2o)
h2o.init(nthreads=-1, max_mem_size= "4g" )

#install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
options(shiny.reactlog = TRUE)
options(max.print=1000000)
ui <- dashboardPage(
  dashboardHeader(title = "prediction of stock returns"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Explore of data", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("linear regression model", tabName = "widgets", icon = icon("bar-chart-o")),
      menuItem("ARIMA", tabName = "ar", icon = icon("book")),
      menuItem("MA with ff 3 factors", tabName = "arff", icon = icon("book")),
      menuItem("h2o random forest", tabName = "h2o", icon = icon("book")),
      menuItem("ML models", tabName = "ml", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                textOutput("text1"),
                tags$head(tags$style("#text1{color: blue;
                align=center;
                style = font-size;
                font-size: 20px;
                font-style: italic;
                                 }"
                )),
                box(#selectInput("symb", "Stock", choices=c(
                  #   "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","JNJ","PG",
                  #     "V","XOM","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                  #    "KO","CSCO","BA","UNH","CMCSA","PFE","WFC","PEP","WMT","MCD",
                  ##   "C","ABT","MDT","ADBE","CRM","IBM","COST","PYPL",
                  #  "ACN","HON","TXN","ORCL","AMGN","PM",
                  #  "UNP","NKE","NFLX","TMO","NEE"
                  #        ), multiple = F, selected = c("AMZN"),
                  #       selectize = TRUE),
                  textInput("symb", "Stock",value = "AMZN"),
                  dateRangeInput("dates",
                                 "Date range",
                                 start = "2014-01-01",
                                 end = "2020-08-30"),
                  actionButton(inputId = "go",
                               label = "Update", icon("refresh")),
                  radioButtons("techstat", "Technical Statistics:",
                               c("Bollinger Bands" = "addBB",
                                 "Volume" = "addVol",
                                 "WW Directional Movement Indicator" = "addADX",
                                 "Chaiken Money Flow" = "addCM"))),
                box(plotOutput("plot")),
                
                helpText("REFERENCES
              [1] stock data: yahoo finance.
              [2].FF 3 factors data: http://mba.tuck.dartmouth.edu/pages/faculty/ken.french
              [3] https://github.com/abhinav314/rchitects_ur4a/blob/master/UI_rchitects_v1.5.R
            	[4] http://www.reproduciblefinance.com/shiny/fama-french-choose-factors/
              [5] https://rpubs.com/crossxwill/time-series-cv"
                )
              )
      ),
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(h2("Prediction tab content"),
                       fluidRow(box(#selectInput(inputId = "frcstStock", label="Stock Options:", choices=c(
                         #                         "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","JNJ","PG",
                         #                        "V","XOM","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                         #                       "KO","CSCO","BA","UNH","CMCSA","PFE","WFC","PEP","WMT","MCD",
                         #                      "C","ABT","MDT","ADBE","CRM","IBM","COST","PYPL",
                         #                     "ACN","HON","TXN","ORCL","AMGN","PM",
                         #                    "UNP","NKE","NFLX","TMO","NEE"
                         #                 ), multiple = F, selected = c("JPM"),selectize = TRUE, width = NULL, size = NULL),
                         textInput("frcstStock", "Stock",value = "JPM"),
                         dateRangeInput("datefc",
                                        "Date range",
                                        start = "2014-01-01",
                                        end = "2020-08-30",
                                        max = "2020-08-30"),
                         textInput("days", "Forecast Period (in days)","100"),
                         textInput("ci", "Confidence Interval","55"),
                         actionButton(inputId = "do",
                                      label = "Update", icon("refresh")),
                         br(),
                       ),box(plotOutput("plot1")),
                       box(verbatimTextOutput("stats"))))),
      tabItem(tabName = "ar",
              fluidRow(h2("ARIMA & correlation within data"),
                       fluidRow(
                         box(textOutput("text22"),
                             tags$head(tags$style("#text22{
                align=center;
                style = font-size;
                font-size: 20px;
                font-style: italicfont-style: italic;
                                 }"
                             )),
                             plotOutput("plot22")),
                         box(plotlyOutput("frcstplot3")) 
                         
                       )
              )),
      # Third tab content
      tabItem(tabName = "arff",
              fluidRow(h2("MA with FF 3 facotrs,please wait for seconds"),
                       fluidRow(
                         valueBoxOutput("progressBox"),
                         valueBoxOutput("approvalBox")),
                       box(highchartOutput("rolling"))%>% withSpinner(color="#0dc5c1"),
                       box(plotOutput("plot11"))%>% withSpinner(color="#0dc5c1"),
                       box(dataTableOutput("ma"))
                       
              )
      ),
      tabItem(tabName = "h2o",
              fluidRow(h2("h2o random forest"),
                       box(verbatimTextOutput("h2o"))
              )
      ),
      tabItem(tabName = "ml",
              fluidRow(h2("ML models take longer time, please wait"),
                       box(plotOutput("frcstplot4"))
              )
      )
      
      
    )
  )
)

server <- function(input, output,session) {
  rquery.cormat<-function(x,
                          type=c('lower', 'upper', 'full', 'flatten'),
                          graph=TRUE,
                          graphType=c("correlogram", "heatmap"),
                          col=NULL, ...)
  {
    library(corrplot)
    # Helper functions
    #+++++++++++++++++
    # Compute the matrix of correlation p-values
    cor.pmat <- function(x, ...) {
      mat <- as.matrix(x)
      n <- ncol(mat)
      p.mat<- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          tmp <- cor.test(mat[, i], mat[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      }
      colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
      p.mat
    }
    # Get lower triangle of the matrix
    getLower.tri<-function(mat){
      upper<-mat
      upper[upper.tri(mat)]<-""
      mat<-as.data.frame(upper)
      mat
    }
    # Get upper triangle of the matrix
    getUpper.tri<-function(mat){
      lt<-mat
      lt[lower.tri(mat)]<-""
      mat<-as.data.frame(lt)
      mat
    }
    # Get flatten matrix
    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
      )
    }
    # Define color
    if (is.null(col)) {
      col <- colorRampPalette(
        c("#67001F", "#B2182B", "#D6604D", "#F4A582",
          "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
          "#4393C3", "#2166AC", "#053061"))(200)
      col<-rev(col)
    }
    
    # Correlation matrix
    cormat<-signif(cor(x, use = "complete.obs", ...),2)
    pmat<-signif(cor.pmat(x, ...),2)
    # Reorder correlation matrix
    ord<-corrMatOrder(cormat, order="hclust")
    cormat<-cormat[ord, ord]
    pmat<-pmat[ord, ord]
    # Replace correlation coeff by symbols
    sym<-symnum(cormat, abbr.colnames=FALSE)
    # Correlogram
    if(graph & graphType[1]=="correlogram"){
      corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
               tl.col="black", tl.srt=45,col=col,...)
    }
    else if(graphType[1]=="heatmap")
      heatmap(cormat, col=col, symm=TRUE)
    # Get lower/upper triangle
    if(type[1]=="lower"){
      cormat<-getLower.tri(cormat)
      pmat<-getLower.tri(pmat)
    }
    else if(type[1]=="upper"){
      cormat<-getUpper.tri(cormat)
      pmat<-getUpper.tri(pmat)
      sym=t(sym)
    }
    else if(type[1]=="flatten"){
      cormat<-flattenCorrMatrix(cormat, pmat)
      pmat=NULL
      sym=NULL
    }
    list(r=cormat, p=pmat, sym=sym)
    cormat
  }
  #tab1
  stockData <- eventReactive(input$update, {
    importData <- lapply(input$stockops, function(symb)
      get.hist.quote(instrument= symb,
                     start = input$dates[1],
                     end = input$dates[2],
                     quote="AdjClose", provider = "yahoo",
                     retclass="zoo"))
    
    importData <- do.call(merge, importData)
    names(importData) <- input$stockops
    importData
  })
  
  dataInput <- eventReactive(input$go, {
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    chartSeries(dataInput(), theme = chartTheme("white"),
                type = "line", TA = NULL, name = input$symb)
    stat <- switch(input$techstat,
                   addBB = addBBands,
                   addVol = addVo,
                   addADX = addADX,
                   addCM = addCMF)
    stat()
  })
  output$text1 <- renderText({
    paste("Select a stock to examine.
                                Information will be collected from Yahoo finance." )
  })
  #tab2
  
  
  forecastData <- eventReactive(input$do, {
    
    stock_close <- lapply(input$frcstStock, function(symb)
      get.hist.quote(instrument= symb,
                     start = input$datefc[1], 
                     end=input$datefc[2],
                     quote="Close", provider = "yahoo",
                     retclass="zoo"))
    stock_close
    download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip", "F-F_Research_Data_Factors_daily_CSV.zip")
    unzip("F-F_Research_Data_Factors_daily_CSV.zip")
    file.remove("F-F_Research_Data_Factors_daily_CSV.zip")
    ff <- read.csv("F-F_Research_Data_Factors_daily.CSV", header=T, skip=3)
    ff = ff[-nrow(ff),]
    ff =  as.data.frame(ff)
    ff[,1]= as.Date(ff[,1],"%Y%m%d")
    stock_close<-as.data.frame(stock_close)
    stock_date<-rownames(stock_close)
    class(stock_date)
    stock_date= ymd(stock_date)
    stock_close<-data.frame(cbind(stock_date,stock_close))
    str(stock_close)
    head(stock_close)
    adjdiff =  data.frame(diff(stock_close[,2]))
    head(adjdiff)
    adjreturn = adjdiff/stock_close[1:nrow(stock_close)-1,2]
    adjreturn = data.frame(cbind(stock_close[2:nrow(stock_close),1],adjreturn))
    selff = subset(ff,ff$X >= adjreturn[1,1] & ff$X <= adjreturn[nrow(adjreturn),1])
    
    adjreturnmrf = as.data.frame(cbind(selff, adjreturn[,2]-selff$RF/100))
    colnames(adjreturnmrf) <- c("X","MKTRF","SMB","HML","RF","RETURNRF")
    adjreturnmrf
    
  })
  
  output$stats<-renderPrint({
    close_data <- as.data.frame(forecastData())
    close_data$Dates <- rownames(close_data)
    rownames(close_data) <- NULL
    lmfit = lm(forecastData()$RETURNRF ~forecastData()$MKTRF +forecastData()$SMB+forecastData()$HML)
    summary(lmfit)
    print(summary(lmfit))
  })
  ##########
  rolling_ff <- eventReactive(input$do, {
    windows<-as.numeric(input$days)
    close_data <- as.data.frame(forecastData())
    rolling_lm <- 
      rollify(.f=function(MKTRF,SMB,HML,RETURNRF){lm(RETURNRF~MKTRF+SMB+HML)},window = windows,unlist=F)
    
    rolling_ff<-close_data %>% 
      #select(MKTRF,SMB,HML,RETURNRF)%>%
      mutate(a=rolling_lm(MKTRF,SMB,HML,RETURNRF))%>%
      slice(-c(1:windows))})
  
  rolling_ff_glance <- eventReactive(input$do, {
    rolling_ff_glance<-  rolling_ff()%>%
      mutate(glanced = map(a, glance)) %>% 
      unnest(glanced)%>% 
      dplyr::select(X, adj.r.squared)})
  
  ############
  output$rolling<-renderHighchart({
    
    rolling_r_squared_xts <- 
      rolling_ff_glance()%>% 
      tk_xts(date_var = X)
    
    
    highchart(type = "stock") %>% 
      hc_title(text = "Rolling adj.R.Squared,using MA with Fama-French 3 factors") %>%
      hc_add_series(rolling_r_squared_xts, color = "cornflowerblue") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_navigator(enabled = FALSE) %>% 
      hc_scrollbar(enabled = FALSE)
    
  })
  
  output$plot1 <- renderPlot({
    close_data <- as.data.frame(forecastData())
    close_data$Dates <- rownames(close_data)
    rownames(close_data) <- NULL
    lmfit = lm(forecastData()$RETURNRF ~forecastData()$MKTRF +forecastData()$SMB+forecastData()$HML)
    results <-data.frame(
      forecastData()$RETURNRF,
      prediciton <-predict(lmfit,forecastData()))
    colnames(results)=c("actual","prediciton")
    p2<-ggplot(results)+geom_density(aes(actual))+geom_density(aes(prediciton,col="red"))+ggtitle("actualVS.prediction(red)")
    p2
  })
  
  #tab3
  forecastD <- eventReactive(input$do, {
    stock_close <- lapply(input$frcstStock, function(symb)
      get.hist.quote(instrument= symb,
                     start = input$datefc[1], 
                     end = input$datefc[2],
                     quote="Close", provider = "yahoo",
                     retclass="zoo"))
    stock_close
  })
  output$frcstplot3 <- renderPlotly({
    stock <- as.data.frame(forecastD())
    stock <- as.xts(stock, .RECLASS=FALSE)
    stock = diff(log(stock),lag=1) 
    stock = stock[!is.na(stock)] #Removing missing values
    breakpoint = floor(nrow(xts(stock))*(0.85))
    b = breakpoint
    stock_train = stock[1:b,]
    stock_test = stock[(b+1):nrow(stock), ]
    fit = arima(stock_train, order = c(2, 0, 2),include.mean=FALSE)
    arima.forecast = forecast(fit, h = as.numeric(input$days),level=as.numeric(input$ci))
    p <- autoplot(arima.forecast, main = "ARIMA Forecast base on stock itself", include = 800, conf.int = TRUE,
                  conf.int.colour = "#0000FF", conf.int.linetype = "none",
                  conf.int.fill = "#000000", conf.int.alpha = 0.5,
                  ylab = "Log return of stock", xlab = "Time (in mins)")
    ggplotly(p)
  })
  
  output$h2o<-renderPrint({
    
    data1 <- forecastData()
    data1 = as.data.frame(data1)
    data1 = subset(data1, select = -c(X,RF))
    datas <- as.h2o(data1)
    
    y <- "RETURNRF"                                # target variable to learn
    x <- setdiff(names(datas),y)                # feature variables are all other columns
    parts <- h2o.splitFrame(datas, 0.8, seed=99) # randomly partition data into 80/20
    train <- parts[[1]]                         # random set of training obs
    test <- parts[[2]]                          # random set of testing obs
    
    randomForest<-h2o.randomForest(x,y = y,
                                   nfolds = 5,
                                   seed = 1111,
                                   keep_cross_validation_predictions = TRUE,
                                   training_frame = train)
    h2o.performance(randomForest)
    
  })
  
  
  
  
  
  
  
  output$frcstplot4 <- renderPlot({
    
    # stock <- as.data.frame(forecastData())
    
    # stock[1,] = as.Date(as.numeric(stock[1,]),origin = "1970-01-01")
    # stock <- as.xts(stock, .RECLASS=FALSE)
    
    #  stock = stock[!is.na(stock)] #Removing missing value
    class(forecastData())
    library(caret)
    #stock$X <- as.factor(month(lubridate::ymd(stock[1,])))
    rowsss =nrow( as.data.frame (forecastData()))
    rowl = rowsss-as.numeric(input$days)-30+2
    rowj=rowsss-as.numeric(input$days)-30+1
    #### creating sampling seeds ####
    
    set.seed(123)
    seeds <- vector(mode = "list", length = rowl)
    for(i in 1:rowj) seeds[[i]] <- sample.int(1000, 5)
    
    seeds[[rowl]] <- sample.int(1000, 1)
    myTimeControl <- trainControl(method = "timeslice",
                                  initialWindow = as.numeric(input$days),
                                  horizon = 30,
                                  fixedWindow = FALSE,
                                  allowParallel = TRUE,
                                  seeds = seeds)
    tuneLength.num <- 5
    
    #  glmnet.mod <- train(RETURNRF ~ .-X,
    #                      data = forecastData(),
    #                     method = "glmnet",
    #                      family = "gaussian",
    #                       trControl = myTimeControl,
    #                        tuneLength=tuneLength.num)
    lm.mod <- train(RETURNRF ~ .-X,
                    data = forecastData(),
                    method = "lm",
                    trControl = myTimeControl,
                    tuneLength=tuneLength.num)
    earth.mod <- train(RETURNRF ~ .-X,
                       data = forecastData(),
                       method = "earth",
                       trControl = myTimeControl,
                       tuneLength=tuneLength.num)
    #  rpart.mod <- train(RETURNRF ~ .-X,
    #                    data = forecastData(),
    #                   method = "rpart",
    #                  trControl = myTimeControl,
    #                 tuneLength=tuneLength.num)
    #  gbm.mod <- train(RETURNRF ~ .-X,
    #   data = forecastData(),
    #   method = "gbm",
    #   distribution="poisson",
    #   trControl = myTimeControl,
    #  tuneLength=tuneLength.num,
    #  verbose=FALSE)
    resamps <- resamples(list(#glmnet = glmnet.mod,
      lm = lm.mod,
      earth=earth.mod
      #  gbm=gbm.mod,
      #rpart=rpart.mod
    ))
    library(lattice)
    
    trellis.par.set(caretTheme())
    (dotplot(resamps, metric = "Rsquared"))
    
    
  })
  
  
  output$plot11 <- renderPlot({
    rolling_ff_tidy <-
      rolling_ff() %>%
      mutate(tidied = map(a, tidy)) %>% 
      unnest(tidied) %>% 
      dplyr::select(X, term, estimate, std.error, statistic, p.value)
    d<-rolling_ff_tidy %>% 
      group_by(term) %>%
      filter(term != "(Intercept)")
    p<-ggplot(d,aes(x = X, y = estimate, color = term)) + 
      geom_line()+ggtitle("Estimated beta using Moving Average with Fama-French 3 factors ")
    p
  })
  output$ma <- renderDataTable({
    rolling_ff_tidy <-
      rolling_ff() %>%
      mutate(tidied = map(a, tidy)) %>% 
      unnest(tidied) %>% 
      dplyr::select(X, term, estimate, std.error,p.value)
    tail(rolling_ff_tidy,n=4)
  })
  output$plot22<-renderPlot({
    close_data <- as.data.frame(forecastData())
    close_data$Dates <- rownames(close_data)
    rownames(close_data) <- NULL
    econmatirx<-rquery.cormat(subset(close_data,select=c(MKTRF,SMB,HML,RETURNRF)),type="full")
    corrplot(econmatirx)
  })
  output$text22<-renderPrint({
    "This is the covariance matrix of raw data.The bigger spot is, the more important as a driver is in regression. "
  })
  output$progressBox <- renderValueBox({
    rolling_ff_glance <-as.data.frame(rolling_ff_glance())
    valueBox(
      paste0(round(mean(rolling_ff_glance$adj.r.squared)*100,2), "%"), "average accuracy", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    rolling_ff_glance <-as.data.frame(rolling_ff_glance())
    
    if(round(mean(rolling_ff_glance$adj.r.squared)*100,2)>70){a="good"}else{
      if(round(mean(rolling_ff_glance$adj.r.squared)*100,2)>50){
        a="not bad"
      }else{a="try other stats"}
    }
    valueBox(
      a, "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
}
shinyApp(ui, server)
