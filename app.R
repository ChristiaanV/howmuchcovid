library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(scales)

style.plot <- function(gg, color) {
  gg +
    geom_line(size = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20), legend.title = element_blank()) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(
      date_minor_breaks = "1 day",
      date_labels = "%b %d"
    ) +
    xlab(NULL) +
    ylab(NULL) +
    scale_color_manual(values = c("1" = color))
}

kOrange <- "#ff851b"
kGreen <-  "#00a65a"
kRed <-    "#dd4b39"
kRefreshMillis <- 1000 * 60 * 5


live <- tags$li(div(id = "headspace", p(id = "rcorners", "LIVE")), class = "dropdown")
header <- dashboardHeader(title = "Coronavirus South Africa", titleWidth = "300", live)
sidebar <- dashboardSidebar(disable = T)
body <- dashboardBody(
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="description", content="Live South African COVID-19 numbers"),
    tags$meta(name="keywords", content="live,South Africa,South African,update,covid,covid-19,coronavirus,map,dashboard"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(rel="shortcut icon", href="https://cdn.countryflags.com/thumbs/south-africa/flag-round-250.png"),
    tags$style(HTML('.skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {background-color: #ecf0f5;}')),
    tags$style(HTML('#rcorners {border-radius: 3px; background: #ff3535; padding-left:5px; padding-right:5px; color: #ffffff; font-size: 17px}')),
    tags$style(HTML('#headspace {padding-top: 13px; padding-right: 13px}'))
  ),
  # Open
  box(
    width = 4,
    column(
      width = 12,
      valueBoxOutput("open_val", width = 12),
    ),
    column(
      width = 12,
      plotOutput("open_plot")
    )

  ),

  # Recovered
  box(
    width = 4,
    column(
      width = 12,
      valueBoxOutput("rec_val", width = 12),
    ),
    column(
      width = 12,
      plotOutput("rec_plot")
    )

  ),
  #Deaths
  box(
    width = 4,
    column(
      width = 12,
      valueBoxOutput("d_val", width = 12),
    ),
    column(
      width = 12,
      plotOutput("d_plot")
    )

  )
)

ui <- dashboardPage(header, sidebar, body, "Coronavirus South Africa", "green")

server <- function(input, output, session) {

  session$allowReconnect(TRUE)

  # South Africa Time Series Data
  getSATS <- reactivePoll(
    kRefreshMillis,
    session,
    function(){
      read.csv("sa_ts.csv")
    },
    function(){
      read.csv("sa_ts.csv") %>%
        mutate(Date = as.Date(paste0(
          "20",
          substr(Date, 7, 8),
          "-",
          substr(Date, 1,2),
          "-",
          substr(Date, 4, 5)
        ))) %>%
        filter(!duplicated(Date)) %>%
        arrange(Date)
    }
  )

  # Values ------------------------------------------------------------------

  # Open
  output$open_val <- renderValueBox({
    val <- tail(getSATS(), 1)$Open
    val <- comma(val)
    valueBox(val, "Open Cases", icon("stethoscope"), "orange", width = 12)
  })

  # Recovered
  output$rec_val <- renderValueBox({
    val <- tail(getSATS(), 1)$Recovered
    val <- comma(val)
    valueBox(val, "Recovered", icon("clipboard-check"), "green", width = 12)
  })

  #Deaths
  output$d_val <- renderValueBox({
    val <- tail(getSATS(), 1)$Deaths
    val <- comma(val)
    valueBox(val, "Deaths", icon("exclamation"), "red", width = 12)
  })


  # Plots -------------------------------------------------------------------

  # Open
  output$open_plot <- renderPlot({
    df <- getSATS()

    df %>%
      mutate(`Open Cases` = Open) %>%
      ggplot(aes(x = Date, y = `Open Cases`, color = "1")) %>%
      style.plot(kOrange)
  })

  # Recovered
  output$rec_plot <- renderPlot({
    df <- getSATS()

    df %>%
      ggplot(aes(x = Date, y = Recovered, color = "1")) %>%
      style.plot(kGreen)
  })

  #Deaths
  output$d_plot <- renderPlot({
    df <- getSATS()

    df %>%
      ggplot(aes(x = Date, y = Deaths, color = "1")) %>%
      style.plot(kRed) +
      scale_y_continuous(limits = c(0,4))
  })
}

shinyApp(ui = ui, server = server, options = c("port" = 3838))
