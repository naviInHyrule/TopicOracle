#MARIFLOR VEGA CARRASCO

library(shiny)
library(ggplot2)  # for the diamonds dataset
library(sqldf)
source("Functions.R")

###########################################################
#### upload data and create pertinent objects
topicWords<-read.csv('Data/random_topic_words.csv',header=T, stringsAsFactors = F)
uniqueTopics=sort(unique(topicWords$topicID))
topicsList=getTopicsList(topicWords)

monthWeights=read.csv('Data/random_year_month.csv',header = T, stringsAsFactors = F)
monthWeights=getExtraVars(monthWeights)
monthsList=getList(monthWeights)

dayWeights=read.csv('Data/random_days.csv',header = T, stringsAsFactors = F)
dayWeights=getExtraVars(dayWeights)
daysList=getList(dayWeights)

hourWeights=read.csv('Data/random_hours.csv',header = T, stringsAsFactors = F)
hourWeights=getExtraVars(hourWeights)
hoursList=getList(hourWeights)

AllWeights=rbind(monthWeights,dayWeights,hourWeights)

plotVarList=list()
plotVarList[['Weights']]<-as.integer(1)
plotVarList[['Proportions']]<-as.integer(2)
plotVarList[['Proportions across Topics']]<-as.integer(3)

plotVarListforTopics=plotVarList
names(plotVarListforTopics)[3] <-'Proportions across Time'

plotTypeLabs=c('Probability','Proportions','Proportion across Topics')
timeVars=c('Monthly','Daily','Hourly')

###########################################################
server <- function(input, output) {
  
#########Plots Topic and its words#########
  
  output$topicProds <- renderUI({
    selectInput("TOPIC", "TopicID:", choices=uniqueTopics)})

  topicInput <- reactive({
    topicProds=getTopicDesc(topicWords,input$TOPIC)
    return(topicProds)})
  
  output$topicProdsPlot <- renderPlot({
    topicProds=topicInput()
    getTopicPlot(topicProds)})
  
#########Plots Topic in Time#########
  
  output$select_topic <- renderUI({
    checkboxGroupInput("SELECTEDTOPIC", "Topics:", topicsList, selected =c('1','13'))})
  
  output$select_plot_t <- renderUI({
    checkboxGroupInput("PLOT_TYPE_TOPIC", "Plot Variable:",plotVarListforTopics, selected = c('1','2','3'))})
  
  selTopicWeights <- reactive({
    AllWeights[(AllWeights$topicID %in% input$SELECTEDTOPIC),]})
  
  output$byTopicPlots <- renderUI({
    plotList=mergeLists(timeVars,input$PLOT_TYPE_TOPIC, 'byTopic')
    plot_output_list <- lapply(plotList,function(plotname){plotOutput(plotname, width = "110%", height = "200px")})
    do.call(tagList, plot_output_list)})
  
    for (j in 1:length(plotTypeLabs)) {
    for (i in timeVars) {
      local({
        varID <- i;plotType<-j
        plotname <- paste0("byTopicPlot",varID,"Type",plotType)
        output[[plotname]] <- renderPlot({
        selWeights=selTopicWeights()
        getTopicInVarPlot(selWeights,varID,plotType)})})}}

#########Plots Monthly#########
  
  output$select_month <- renderUI({
    checkboxGroupInput("MON", "Months:", monthsList, selected ='1')})
  
  output$select_plot_m <- renderUI({
    checkboxGroupInput("PLOT_TYPE_MOM", "Plot Variable:",plotVarList, selected = c('1','2','3'))})
  
  output$monthlyPlots <- renderUI({
    plotList=mergeLists(input$MON,input$PLOT_TYPE_MOM, 'monthly')
    plot_output_list <- lapply(plotList,function(plotname){plotOutput(plotname, width = "110%", height = "200px")})
    do.call(tagList, plot_output_list)})
  
  #I've try to wrap this in a function, but no success. sorry!
  for (i in 1:length(monthsList)) {
    for (j in 1:length(plotTypeLabs)) {
      local({
        varID <- i;plotType<-j
        plotname <- paste0("monthlyPlot", varID,"Type",plotType)
        output[[plotname]] <- renderPlot({getVarPlot(monthWeights, varID, plotType)})})}}

#########Plots Daily#########
  output$select_day <- renderUI({
    checkboxGroupInput("DAY", "Days:", daysList, selected ='1')})
  
  output$select_plot_d <- renderUI({
    checkboxGroupInput("PLOT_TYPE_DAY", "Plot Variable:",plotVarList, selected = c('1','2','3'))})
  
  output$dailyPlots <- renderUI({
    plotList=mergeLists(input$DAY,input$PLOT_TYPE_DAY, 'daily')
    plot_output_list <- lapply(plotList,function(plotname) {plotOutput(plotname, width = "110%", height = "200px")})
    do.call(tagList, plot_output_list)})

  for (i in 1:length(daysList)) {
    for (j in 1:length(plotTypeLabs)) {
      local({
        varID <- i;plotType<-j
        plotname <- paste0("dailyPlot", varID,"Type",plotType)
        output[[plotname]] <- renderPlot({getVarPlot(dayWeights, varID, plotType)})})}}
  
#########Plots Hourly#########
  
  output$select_hour <- renderUI({
    checkboxGroupInput("HOUR", "Hours:", hoursList,  selected = '8')})
  
  output$select_plot_h <- renderUI({
    checkboxGroupInput("PLOT_TYPE_HOUR", "Plot Variable:",plotVarList, selected = c('1','2','3'))})
  
  output$hourlyPlots <- renderUI({
    plotList=mergeLists(input$HOUR,input$PLOT_TYPE_HOUR, 'hourly')
    plot_output_list <- lapply(plotList,function(plotname) {plotOutput(plotname, width = "110%", height = "200px")})
    do.call(tagList, plot_output_list)})
  
  for (i in 1:length(hoursList)) {
    for (j in 1:length(plotTypeLabs)) { 
      local({
        varID <- i;plotType<-j
        plotname <- paste0("hourlyPlot", varID,"Type",plotType)
        output[[plotname]] <- renderPlot({getVarPlot(hourWeights, varID, plotType)})})}}
}

#runApp(appDir = getwd(),launch.browser = TRUE)
  
