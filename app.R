# Load the Shiny package and ggplot2
library(shiny)
library(ggplot2)
library(scales)

# Define UI for the app
ui <- fluidPage(
  titlePanel(
    div(
      tags$img(height = "50px", style = "margin-right: 0px;"),
      "Golfsagan - Birdie bucket"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("player_name", "Player Name:"),
      numericInput("handicap", "Handicap:", value = 0, min = 0, step = 1),
      numericInput("birdie_count", "Birdie Count:", value = 0, min = 0, step = 1),
      actionButton("add_player", "Add Player"),
      br(),
      br(),
      actionButton("draw_winner", "Draw Winner"),
      br(),
      br(),
      tableOutput("player_table"),
      br(),
      div(textOutput("winner_name"), style = "font-size: 20px;"),  # Larger font size for winner text
      br(),
      textOutput("last_three_winners")  # Display last three winners
    ),
    
    mainPanel(
      h4("Instructions:"),
      p("1. Enter the player's name, handicap, and birdie count, then click 'Add Player'."),
      p("2. To remove a player, click the 'Remove' button next to their name."),
      p("3. After updating all players, click 'Draw Winner' to randomly select a winner."),
      p("4. The probability of winning is based on the sum of the player's handicap and birdies."),
      plotOutput("prob_plot")
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  # Reactive values to store player data and last three winners
  players <- reactiveVal(data.frame(
    Name = character(),
    Handicap = numeric(),
    Birdies = numeric(),
    Tickets = numeric(),
    stringsAsFactors = FALSE
  ))
  
  last_three_winners <- reactiveVal(character(0))
  
  # Add player data to the reactive values
  observeEvent(input$add_player, {
    players_df <- players()
    if (input$player_name %in% players_df$Name) {
      # Update existing player's data
      index <- which(players_df$Name == input$player_name)
      players_df$Handicap[index] <- input$handicap
      players_df$Birdies[index] <- input$birdie_count
      players_df$Tickets[index] <- input$handicap + input$birdie_count
    } else {
      # Add new player data
      new_player <- data.frame(
        Name = input$player_name, 
        Handicap = input$handicap, 
        Birdies = input$birdie_count, 
        Tickets = input$handicap + input$birdie_count,
        stringsAsFactors = FALSE
      )
      players_df <- rbind(players_df, new_player)
    }
    players(players_df)
  })
  
  # Remove player data from the reactive values
  observeEvent(input$draw_winner, {
    player_data <- players()
    total_tickets <- sum(player_data$Tickets)
    
    if (total_tickets > 0) {
      winner_index <- sample(1:nrow(player_data), size = 1, prob = player_data$Tickets)
      winner <- player_data$Name[winner_index]
      
      # Update last three winners
      last_three <- last_three_winners()
      if (length(last_three) >= 3) {
        last_three <- c(last_three[-1], winner)
      } else {
        last_three <- c(last_three, winner)
      }
      last_three_winners(last_three)
    } else {
      winner <- "No players added."
    }
    
    output$winner_name <- renderText({
      paste("Winner: ", winner)
    })
  })
  
  # Remove player event observers
  observeEvent(input$remove_player, {
    player_name <- gsub("remove_", "", input$remove_player)
    players_df <- players()
    players_df <- players_df[players_df$Name != player_name, ]
    players(players_df)
  })
  
  # Display the player table with remove buttons
  output$player_table <- renderTable({
    players_df <- players()
    if (nrow(players_df) > 0) {
      players_df$Remove <- sapply(players_df$Name, function(name) {
        as.character(actionButton(inputId = paste0("remove_", name), label = "X", onclick = sprintf('Shiny.onInputChange("%s", "%s")', "remove_player", paste0("remove_", name))))
      })
    }
    players_df
  }, sanitize.text.function = function(x) x)
  
  # Display last three winners
  output$last_three_winners <- renderText({
    last_three <- last_three_winners()
    if (length(last_three) > 0) {
      paste("Last Three Winners:", paste(last_three, collapse = ", "))
    } else {
      ""
    }
  })
  
  # Plotting the probability mass function
  output$prob_plot <- renderPlot({
    player_data <- players()
    if (nrow(player_data) > 0) {
      player_data <- player_data[order(player_data$Name), ]
      player_data$Handicap_prop <- player_data$Handicap / sum(player_data$Tickets)
      player_data$Birdies_prop <- player_data$Birdies / sum(player_data$Tickets)
      
      ggplot(player_data, aes(x = Name)) +
        geom_bar(aes(y = Handicap_prop+Birdies_prop, fill = "Handicap"), stat = "identity", position = "stack", width = 0.5) +
        geom_bar(aes(y = Birdies_prop, fill = "Birdies"), stat = "identity", position = "stack", width = 0.5) +
        scale_fill_manual(values = c("Handicap" = "steelblue", "Birdies" = "tomato"), name = "Contributions") +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(title = "Probability Mass Function",
             x = "Player",
             y = "Probability") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(size = 15))
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
