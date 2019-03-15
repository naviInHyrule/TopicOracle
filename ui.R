#MARIFLOR VEGA CARRASCO

ui <- fluidPage(
  headerPanel("Topics in Time"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Topics in Time"',
        htmlOutput("select_topic"),
        htmlOutput("select_plot_t")),
      conditionalPanel(
        'input.dataset === "Monthly"',
        htmlOutput("select_month"),
        htmlOutput("select_plot_m")),
      conditionalPanel(
        'input.dataset === "Daily"',
        htmlOutput("select_day"),
        htmlOutput("select_plot_d")),
      conditionalPanel(
        'input.dataset === "Hourly"',
        htmlOutput("select_hour"),
        htmlOutput("select_plot_h")),
      hr(),
      htmlOutput("topicProds"),
      plotOutput("topicProdsPlot",width = "100%", height = "800px"),
      width = 3),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Topics in Time",fluidRow(
                   column(12, uiOutput("byTopicPlots")))),
        tabPanel("Monthly", fluidRow(
          column(12, uiOutput("monthlyPlots")))),
        tabPanel("Daily", fluidRow(
          column(12, uiOutput("dailyPlots")))),
        tabPanel("Hourly", fluidRow(
          column(12, uiOutput("hourlyPlots"))))))))
