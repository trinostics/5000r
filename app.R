library(shiny)
library(shinyjs)

source("score.r")

DODGERBLUE <- "1E90FF"
pickColor <- c("1E90FF", #DODGERBLUE
               "CD5C5C", #INDIANRED
               "00FF7F", #SPRINGGREEN
               "BA55D3", #MEDIUMORCHID
               "BC8F8F"  #ROSYBROWN
)

new_selectionSet <- function(v = integer(0), index = seq_along(v), 
                             color = rep(DODGERBLUE, length(v))) {
  structure(v, index = index, color = color, class = "selectionSet")
}

new_turn <- function() {
  structure(list(),
            firstroll = TRUE,
            class = "turn") # list of saved selection sets
}
new_player <- function(x, name = "A") {
  structure(list(), 
            name = name,
            scoredYet = FALSE,
            class = "player") # list of turns
}

ui <- fluidPage(
  includeScript(file.path("www", "enter_button.js")),
  useShinyjs(),
   
   titlePanel("5000 Dice Game"),
   
   sidebarLayout(
      sidebarPanel(
#        actionButton("newplayer", "New Player")
#        , actionButton("newturn", "New Turn")
#        , br()
#        , actionButton("roll", "Roll")
#        , actionButton("endturn", "End Turn")
        h2("Score")
        , h3("Roll Area")
        , verbatimTextOutput("scoreRollArea")
        , h3("Picked")
        , verbatimTextOutput("scorePicked")
        , h3("Not Picked")
        , verbatimTextOutput("scoreNotPicked")
        , h3("Saved")
        , verbatimTextOutput("scoreSaved")
        , h3("On Table (= 'turn')")
        , verbatimTextOutput("scoreTable")
        , h3("Player A")
        , verbatimTextOutput("scorePlayerA")
        , p("# turns")
        , verbatimTextOutput("numturns")
      ),
      mainPanel(
        uiOutput("playingSurface")
      )
   )

)
initialize_savedSelectionSets <- function() {
  structure(list(), class = "savedSelectionSets")
}

server <- function(input, output) {
  WHITE <- "FFFFFF"
  NDICEFULLROLL <- 5
  nDiceToRoll <<- NDICEFULLROLL
  WINNINGSCORE <- 5000
  DieSelectionColor <- "1E90FF" #DODGERBLUE
  
  rval <- reactiveValues()
  
  # first player and turn
  rval$turn <- new_turn()
  rval$player <- new_player(rval$turn, name = "A")

  rval$selectionSet <- new_selectionSet()
  rval$savedSelectionSets <- initialize_savedSelectionSets()
  
  rval$rollArea <- data.frame(
    value = integer(0),
    picked = logical(0),
    color = character(0)
  )
  output$errmsg1 <- NULL
  output$scoreRollArea <- renderText({score(rval$rollArea$v)})
  output$scorePicked <- renderText({score(rval$selectionSet)})
  output$scoreNotPicked <- renderText({
    ndx <- attr(rval$selectionSet, "index")
    z <- rval$rollArea$v[-ndx]
    score(z)
  })
  output$scoreSaved <- renderText(score(rval$savedSelectionSets))
  output$scoreTable <- renderText(score(rval$selectionSet) + score(rval$savedSelectionSets))
  output$scorePlayerA <- renderText(score(rval$player))
  output$numturns <- renderText(length(rval$player))
  rval$diceRolled <- reactive({
    lapply(1:nrow(rval$rollArea), 
      function(i) actionButton(inputId = paste0("d", i), 
        label = img(src = paste0("D", rval$rollArea$value[i], ".jpg")),
        style=paste0("background-color: #", rval$rollArea$color[i])
        ))
  })

  output$playingSurface <- renderUI({
    fluidPage(
      h2("rollArea"),
      actionButton("roll", "Roll"),
      actionButton("endturn", "End Turn"), 
      ifelse(nrow(rval$rollArea), 
             ifelse(score(rval$valuesRolled), "Select dice", "No points. Click End Turn"), 
             "click Roll"),
      br(),
      if (nrow(rval$rollArea)) do.call(fluidRow, rval$diceRolled()),
      htmlOutput("errmsg2"),
      if (length(rval$savedSelectionSets)) h2("savedArea"),
      if (length(rval$savedSelectionSets)) renderTable({
        M <- plyr::ldply(isolate(rval$savedSelectionSets), rbind)
        M[is.na(M)] <- " "
        colnames(M) <- paste0("D", seq(ncol(M)))
        M
        })
      )
    })

  observeEvent(input$newplayer, {
    rval$player <- new_player("A")
    add_player_to_Players(rval$player)
  })

  observeEvent(input$newturn, {
    rval$turn <- new_turn()
  })

  observeEvent(input$roll, {
    isolate({
      if (!attr(rval$turn, "firstroll")) {
        # If already rolled once this turn, cannot roll again without picking something
        if (length(rval$selectionSet) < 1L) {
          output$errmsg2 <- renderText(
            paste("<span style=\"color:red\">At least one pointed die must be selected.</span>"))
          return()
        }
      }
      z <- attr(score(rval$selectionSet), "diceremaining")
      # "diceremaining" are the dice in selectionSet that do not contribute to the score
      if (length(z)) {
        output$errmsg2 <- renderText(
          paste("<span style=\"color:red\">All dice selected must contain points.</span>"))
        return()
      }
    })
    output$errmsg1 <- NULL
    output$errmsg2 <- NULL
    attr(rval$turn, "firstroll") <- FALSE
    add_selectionSet_to_savedSelectionSets()
    initialize_selectionSet()
    v <- sample.int(6, nDiceToRoll, TRUE) #1:nDiceToRoll 
    rval$rollArea <- data.frame(
      value = v,
      picked = FALSE,
      color = WHITE,
      stringsAsFactors = FALSE
    )
    rval$valuesRolled <- v
  })
  observeEvent(input$endturn, {
    if (score(rval$rollArea$v) == 0) { # zilch!
      rval$rollArea <- data.frame(
        value = integer(0),
        picked = logical(0),
        color = character(0)
      )
      nDiceToRoll <<- NDICEFULLROLL
      rval$selectionSet <- new_selectionSet()
      rval$savedSelectionSets <- initialize_savedSelectionSets()
      rval$turn <- new_turn()
      add_Turn_to_Player()
      output$errmsg2 <- NULL
      return()
    }
    if (attr(rval$player, "scoredYet")){
      if (score(rval$savedSelectionSets) + score(rval$selectionSet) < 300) {
        output$errmsg2 <- renderText(
          paste("<span style=\"color:red\">You must score at least 300 to end turn.</span>"))
        return()
      }
    }
    else
      if (score(rval$savedSelectionSets) + score(rval$selectionSet) < 500) {
        output$errmsg2 <- renderText(
          paste("<span style=\"color:red\">You must first score at least 500 to end turn.</span>"))
        return()
      }
    add_selectionSet_to_savedSelectionSets()
    attr(rval$player, "scoredYet") <- TRUE
    add_savedSelectionSets_to_Turn()
    add_Turn_to_Player()
    rval$rollArea <- data.frame(
      value = integer(0),
      picked = logical(0),
      color = character(0)
    )
    nDiceToRoll <<- NDICEFULLROLL
    rval$selectionSet <- new_selectionSet()
    rval$savedSelectionSets <- initialize_savedSelectionSets()
    if (score(rval$player) >= WINNINGSCORE) {
      output$errmsg2 <- renderText(paste(
        "<span style=\"color:blue; font-size:large\">Player", 
        attr(rval$player, "name"),
        "is the winner in ", 
        length(rval$player),
        ifelse(length(rval$player) > 1L, "turns</span>", "turn! Wow!!!</span>")
      ))
    }
    rval$turn <- new_turn()
  })
  
  observeEvent(input$d1, pickedADie(1))
  observeEvent(input$d2, pickedADie(2))
  observeEvent(input$d3, pickedADie(3))
  observeEvent(input$d4, pickedADie(4))
  observeEvent(input$d5, pickedADie(5))
  
  pickedADie <- function(picked) {
    output$errmsg2 <- NULL
    nDiceToRoll <<- nDiceToRoll + ifelse(rval$rollArea$picked[picked],
                                         1L, -1L)
    if (nDiceToRoll < 1L) nDiceToRoll <<- NDICEFULLROLL
    rval$rollArea$picked[picked] <- !rval$rollArea$picked[picked]
    rval$rollArea$color[picked] <- ifelse(rval$rollArea$picked[picked],
                                          DODGERBLUE, WHITE)
    if (rval$rollArea$picked[picked]) add_Die_to_selectionSet(picked)
    else remove_Die_from_selectionSet(picked)
  }

  add_selectionSet_to_savedSelectionSets <- function(){
    if (length(rval$selectionSet)) 
      rval$savedSelectionSets[[length(rval$savedSelectionSets) + 1]] <- rval$selectionSet
  }
  initialize_selectionSet <- function() {
    rval$selectionSet <- new_selectionSet()
  }
  add_Die_to_selectionSet <- function(picked) {
    rval$selectionSet <- if (length(rval$selectionSet)) new_selectionSet(
      c(c(rval$selectionSet), rval$valuesRolled[picked]),
      index = c(attr(rval$selectionSet, "index"), picked),
      color = attr(rval$selectionSet, "color"))
    else new_selectionSet(
      rval$valuesRolled[picked],
      index = picked,
      color = DODGERBLUE)
  }
  remove_Die_from_selectionSet <- function(picked) {
    w <- which(attr(rval$selectionSet, "index") == picked)
    rval$selectionSet <- new_selectionSet(
      rval$selectionSet[-w],
      index = attr(rval$selectionSet, "index")[-w],
      color = attr(rval$selectionSet, "color")[-w]
    )
  }
  
  add_savedSelectionSets_to_Turn <- function() {
    n <- length(rval$turn)
    rval$turn[[n+1]] <- rval$savedSelectionSets
  }
  add_player_to_Players <- function(p) {
    playernames <- sapply(rval$Players, function(x) attr(x, "name"))
    nam <- attr(p, "name")
#    if (nam %in% playernames) stop(nam, "already a player")
    rval$Players[[length(rval$Players) + 1L]] <- p
  }
  
  add_Turn_to_Player <- function() {
    rval$player[[length(rval$player) + 1L]] <- rval$turn
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

