

#load packages
library(shiny)
library(tidyverse)
library(png)
library(DT)
library(shinyWidgets)

#court images
court_white <- readPNG("court.png")
court_yellow <- readPNG("court_yellow.png")
court_purple <- readPNG("court_purple.png")
court_blue <- readPNG("court_blue.png")
court_gray <- readPNG("court_gray.png")
court_pink <- readPNG("court_pink.png")
court_black <- readPNG("court_black.png")

#callback for table
callback <- c('$("#remove").on("click", function(){','  table.rows(".selected").remove().draw();','});')

#UI
ui <- basicPage(
  
  setBackgroundColor("ghostwhite"),
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }"),
  tags$style(type="text/css", ".rotate90 {
                                          -webkit-transform: rotate(180deg);
                                          -moz-transform: rotate(180deg);
                                          -o-transform: rotate(180deg);
                                          -ms-transform: rotate(180deg);
                                           transform: rotate(180deg);}"),
  
  br(),
  br(),
  
  fluidRow(
    
    column(width = 2, uiOutput("input_home")),
    column(width = 7),
    column(width = 1, actionButton("remove", "Deleted Selected Rows", icon = icon("trash"))),
    column(width = 2, div(style = "padding-left: 33px; padding-top: 8px", uiOutput("selectall")))
    
  ),
  
  hr(),
  
  fluidRow(
    column(width = 1, uiOutput("input_court")),
    column(width = 1, uiOutput("input_color")),
    column(width = 3,  div(style = "padding-left: 33px;", uiOutput("input_player"))),
    column(width = 2, uiOutput("input_period")),
    column(width = 3, uiOutput("input_event")),
    column(width = 2, div(style = "padding-left: 33px;", uiOutput("input_phase")))
  ),
  
  hr(),
  
  fluidRow(
    column(width = 3,  div(style = "padding-left: 60px;", plotOutput("plot_court", click = "plot_click", width = "350px", height="700px"))),
    column(width = 8, DTOutput("mytable"))
  ),
  
  absolutePanel(HTML(paste("Built by Jose Fernandez | 
                           Twitter: ", a("@jfernandez__", href="https://twitter.com/jfernandez__"), " | 
                           GitHub: ", a("josedv82", href="https://github.com/josedv82"), " | ",
                           a("App Code", href="https://github.com/josedv82/basketball_event_tracker"))), 
                bottom = 0, right = 10, fixed = TRUE)
  
  
)

#server
server <- function(input, output, session) {
  
  
  #click inputs
  mytable <- data.frame(
    Team = character(),
    Player = character(),
    Period = character(),
    Phase = character(),
    Event = character(),
    `Location X` = numeric(), 
    `Location Y` = numeric(), 
    Color = character(),
    Shape = numeric(),
    Notes = character(),
    ID = numeric(), 
    check.names = FALSE
  )
  
  
  ID = reactiveVal(0L)
  team = reactiveVal()
  player = reactiveVal()
  period = reactiveVal()
  phase = reactiveVal()
  event = reactiveVal()
  xcoords = reactiveVal()
  ycoords = reactiveVal()
  color = reactiveVal()
  shape = reactiveVal()
  
  
  
  #input select all rows
  output$selectall <- renderUI({
  materialSwitch(inputId = "select_all", label = "Select All Rows", right = T)
  })
  
  
  #input home
  output$input_home <- renderUI({
    
    radioGroupButtons(
      inputId = "home",
      label = NULL,
      choices = c("Home", "Away"),
      status = "primary",
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    )
    
  })
  
  output$input_phase <- renderUI ({
    
    radioGroupButtons(
      inputId = "phase",
      label = "Select Phase",
      choices = c("Offense", "Defense"),
      selected = "Offense"
    )
    
    
  })
  
  
  #create event inputs
  output$input_event <- renderUI({
    
    req(input$phase)
    
    Off <- c("Shot Made", "Shot Missed", "Pass", "Assist", "Turnover", "Other") 
    Def <- c("Rebound", "Block", "Steal", "Fault", "Other")
    
    radioGroupButtons(
      inputId = "event",
      label = "Select Event", 
      choices = if (input$phase == "Offense") Off else Def,
      selected = NULL,
      individual = TRUE
    )
    
  })
  
  #create list of players
  output$input_player <- renderUI({
    
    radioGroupButtons(
      inputId = "player",
      label = "Select Player", 
      choices = c("Player 1", "Player 2", "Player 3", "Player 4", "Player 5"),
      selected = "Player 1",
      individual = TRUE
    )
    
  })
  
  #create list periods
  output$input_period <- renderUI({
    
    radioGroupButtons(
      inputId = "period",
      label = "Select Period", 
      choices = c("1st", "2nd", "3rd", "4th", "OT"),
      selected = "1st",
      individual = TRUE
    )
    
  })
  
  #create list courts
  output$input_court <- renderUI({
    
    pickerInput(
      inputId = "court",
      label = "Court Background", 
      choices = c("white", "yellow", "purple", "black", "pink", "gray", "blue"),
      choicesOpt = list(style = rep(("color: black; background: white"),7), 
                        content = c(sprintf("<img src = 'court.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", ""),
                                    sprintf("<img src = 'court_yellow.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", ""),
                                    sprintf("<img src = 'court_purple.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", ""),
                                    sprintf("<img src = 'court_black.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", ""),
                                    sprintf("<img src = 'court_pink.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", ""),
                                    sprintf("<img src = 'court_gray.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", ""),
                                    sprintf("<img src = 'court_blue.png' width=45px class='rotate90'><div class='jhr'>%s</div></img>", "")
                        )),
      multiple = FALSE
    )
    
    
  })
  
  #create list courts
  output$input_color <- renderUI({
    
    pickerInput(
      inputId = "color",
      label = "Marker Color", 
      choices = c("white", "yellow", "black", "red", "green", "blue"),
      choicesOpt = list(
        content = c("<div style='color: #e6e6e6; text-align: middle;'>White</div>", 
                    "<div style='color: #fcba03; text-align: middle;'>Yellow</div>",  
                    "<div style='color: black; text-align: middle;'>Black</div>", 
                    "<div style='color: red; text-align: middle;'>Red</div>", 
                    "<div style='color: green; text-align: middle;'>Green</div>",
                    "<div style='color: blue; text-align: middle;'>Blue</div>")),
      multiple = FALSE,
      selected = "black"
    )
    
  })
  
  
  
  
  
  #bind clicks
  observeEvent(input$plot_click, {
    
    
    xcoords(c(xcoords(), input$plot_click$x))
    ycoords(c(ycoords(), input$plot_click$y))
    team(c(team(), input$home))
    player(c(player(), input$player))
    period(c(period(), input$period))
    phase(c(phase(), input$phase))
    event(c(event(), input$event))
    color(c(color(), input$color))
    
    shape(c(shape(), if (input$event == "Shot Made") 19 
                  else if (input$event == "Shot Missed") 10
                  else if (input$event == "Pass") 17
                  else if (input$event == "Turnover") 14
                  else if (input$event == "Assist") 8
                  else if (input$event == "Rebound") 12
                  else if (input$event == "Steal") 11
                  else if (input$event == "Block") 6
                  else if (input$event == "Fault") 5
                  else 15))
    
    newRow <- as.data.frame(list(ID() + 1L, input$home, input$player, input$period, input$phase, input$event, round(input$plot_click$x,2), round(input$plot_click$y,2), ""))
    ID(ID() + 1L)
    addRow(proxy, newRow, resetPaging = FALSE)
    
  }) 
  
  
  
  #interactive court
  output$plot_court <- renderPlot({
    
    req(input$court)
    
    
    court <- if (input$court == "white") court_white 
    else if (input$court == "yellow") court_yellow
    else if (input$court == "purple") court_purple
    else if (input$court == "black") court_black
    else if (input$court == "pink") court_pink
    else if (input$court == "blue") court_blue
    else if (input$court == "gray") court_gray
    else NULL
    
    
    
    # Set up a plot area with no plot
    par(bg = 'black', mar = c(0, 0, 0, 0), xpd = TRUE)
    plot(c(-40, 40), c(-80, 80), type = "n", axes = F , ylab = "", xlab = "")
    rasterImage(court, xleft = -40, xright = 40, ybottom = -80, ytop = 80)
    points(xcoords(), ycoords(), pch = shape(), cex = 2, col = color())
    #legend("bottomright", inset = c(-0.05, 0), bty = "n", cex = 0.9, pt.cex = 1.3, text.col = "darkgrey", col = "darkgrey", ncol = 5,
    #       legend = c("Shot Made","Shot Missed", "Pass", "Assist", "Turnover"), 
    #       pch = c(19, 10, 17, 14, 8))
    
    
  })
  
  #table
  output$mytable <- renderDT({
    
    
     datatable(mytable %>% select(`Play Id` = ID, Team, Player, Period, Phase, Event, `Location X`, `Location Y`, Notes),
              editable = list(target = "cell", disable = list(columns = 0:7)),
              filter = list(position = 'top', clear = FALSE, plain = TRUE),
              extensions = 'Buttons',
              options = list(pageLength = 15, 
                             autoWidth = TRUE,
                             dom = 'Btp',
                             buttons = c('copy', 'csv', 'excel', "pdf"),
                             columnDefs = list(list(className = 'dt-center', targets = 0:8)),
                             order = list(0, 'desc'),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")),
              rownames= F,
              callback = JS(callback)
              ) %>%
      
      formatStyle('Event', target = 'cell',
                  color = styleEqual(c("Shot Made", "Shot Missed", "Pass", "Turnover", "Assist", "Other", "Rebound", "Steal", "Block", "Fault"),
                                     c('green', 'red', 'green', "red", "green", "gray", "green", "green", "green", "red"))
      ) %>%
      
      formatStyle('Team', target = 'cell',
                  color = styleEqual(c("Home", "Away"),
                                     c('blue', 'red'))
      ) %>%
      
      formatStyle('Phase', target = 'cell',
                  color = styleEqual(c("Offense", "Defense"),
                                     c('blue', 'red'))
      )
      
    
  }, server = FALSE)
  
  proxy <- dataTableProxy("mytable")
  
  # remove btn
  observeEvent(input$remove, {
    req(input$mytable_rows_selected)
    indices <- input$mytable_rows_selected
    xcoords(xcoords()[-indices])
    ycoords(ycoords()[-indices])
    team(team()[-indices])
    player(player()[-indices])
    period(period()[-indices])
    phase(phase()[-indices])
    event(event()[-indices])
    color(color()[-indices])
    shape(shape()[-indices])
    
  })
  
  
  observeEvent(input$select_all, {
    if (isTRUE(input$select_all)) {
      selectRows(proxy, input$mytable_rows_all)
    } else {
      selectRows(proxy, NULL)
    }
  })
  
  
}

shinyApp(ui, server)
