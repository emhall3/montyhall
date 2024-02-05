#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#'   Select a door for the Monty Hall Problem.
#'
#' @description
#'   `select_door()` is a function that returns a list of door values 
#'   set to the size you specify.
#'
#' @details
#'   This function works by drawing from a random sample of three numeric 
#'   values and selecting one of these possible values as the contestant's 
#'   initial door choice.
#'
#' @param Doors is a numeric vector used to identify each door.
#' @param Size  The default size for this game is 1.
#' @param Replace is set to False so results are not written over. 
#' 
#' @return The output returns one of three possible door values. This is the
#'   contestant's initial door choice.
#'
#' @examples
#'   select_door()
#'    
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens goat door in the Monty Hall Problem.
#'
#' @description
#'   `open_goat_door()` indicates the host's selection of goat door 
#'   within the game.
#'
#' @details
#'   This function returns the number of one of the possible goat doors.
#'   The function works by eliminating the possibility of the host selecting
#'   either the contestant's selected door or the door with a car behind it.
#'
#' @param Doors is a numeric vector. Refer to the function 'select_door'
#' @param Game is a character vector. Refer to the function 'create_game'
#' @param a.pick is the value of the door selected by the contestant
#' @param available.doors provides the available goat door options, 
#' eliminating the door with the car behind it and eliminating the door 
#' that the contestant selected.
#' 
#' @return The output returns the number of one of the goat doors.
#'   This door is eliminated as an option for the contestant to choose 
#'   for the rest of the game.
#'
#' @examples
#'   this.game <- create_game()
#'   this.game
#'   my.initial.pick <- select_door()
#'   my.initial.pick
#'   open_goat_door( this.game, my.initial.pick )
#'    
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   Allow the contestant to change doors in the Monty Hall Problem.
#'
#' @description
#'   `change_door()` is a function that gives the contestant the option 
#'   of selecting the other remaining door or sticking with the door 
#'   they originally selected.
#'
#' @details
#'   This function returns the number of the contestant's final choice. 
#'   The function works by eliminating the option of the door opened by the host.
#'   If the contestant chooses to stay with their selection (stay = T), the 
#'   final door will be equal to the original selection. If the contestant 
#'   chooses to switch doors (stay = F), the function also eliminates the 
#'   option of the door previously selected by the contestant.
#'
#' @param stay is a logical vector that indicates whether the contestant chose 
#' to stick with their selection (TRUE) or change their selection (FALSE)
#' @param Doors is a numeric vector. Refer to the function 'select_door'
#' @param a.pick is the numeric value of the door selected by the contestant in 
#' the game.
#' @param opened.door is the numeric value of the door opened in the 
#' 'open_goat_door' function.
#' 
#' @return The output is the numeric value of the contestant's final 
#'   door selection.
#'
#' @examples
#'   opened.door <- open_goat_door( this.game, my.initial.pick )
#'   change_door( stay=T, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   change_door( stay=F, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determines winner of the Monty Hall Problem game.
#'
#' @description
#'   `determine_winner()` is a function that indicates whether the contestant 
#'   won or lost the game.
#'
#' @details
#'   This function works using an if statement. If the contestant's final
#'   door selection contains the car, the contestant wins. If the contestant's 
#'   final door selection contains the goat, the contestant loses.
#'
#' @param Game is a character vector. Refer to the function 'create_game'
#' @param final.pick is the numeric value of the final door selection.
#' 
#' @return The output will be WIN if the final door contains the car.
#'   The output will be LOSE if the final door contains the goat.
#'
#' @examples
#'   this.game
#'   my.initial.pick
#'   my.final.pick <- change_door( stay=T, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, 
#'   game=this.game )
#'   my.final.pick <- change_door( stay=F, 
#'   opened.door=opened.door, 
#'   a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, 
#'   game=this.game )
#'   
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#'   Plays the entire Monty Hall Problem game.
#'
#' @description
#'   `play_game()` is a function that combines all of the functions needed 
#'   to create the Monty Hall game for ease of testing with a large 
#'   sample size.
#'
#' @details
#'   This function works by combining the create_game, select_door, 
#'   open_goat_door, change_door, and determine_winner functions 
#'   so they run as one.
#'
#' @param Strategy is a character vector that indicates whether the contestant
#' chose to switch doors or stay with the same door.
#' @param Outcome is a character vector that shows the outcome of the game 
#' if the contestant stays and the outcome of the game if the contestant 
#' switches.
#' 
#' @return The output is a data frame called 'Game Results' that shows
#'   the outcomes of the game if the contestant switches and if the 
#'   contestant stays.
#'
#' @examples
#'   play_game()
#'   
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Plays the Monty Hall Problem n number of times.
#'
#' @description
#'   `play_n_games()` plays the Monty Hall game a set number of times 
#'   to see the average number of times a contestant will win depending 
#'   on their strategy.
#'
#' @details
#'   This function works by using a for loop to play the game a certain 
#'   number of times. The results are collected into a list and analyzed 
#'   to determine the average number of wins and losses per strategy. The 
#'   package dplyr is required to perform this analysis.
#'
#' @param results.list collects the results of the game loop
#' @param n the number of times the game is played within the loop
#' @param i iterator that plays the game n number of times
#' @param game.outcome places the play_game function into the loop
#' @param bind_rows binds the results and game outcome into the results list
#' 
#' @return The output is a table that displays how many times the 
#'   contestant won or lost the game for each strategy. The result 
#'   is rounded to two decimal points for ease of viewing.
#'
#' @examples
#'   play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}