#Tennis Functions

calculate_elo <- function(data, k = 32, starting_elo = 1500) {
  
  # create starting elo ratings for every player
  players <- unique(c(data$winner_name, data$loser_name))
  players_elo <- data.frame(players = players, elo = rep(starting_elo, length(players)))
  
  # empty columns for pre-match elo ratings
  data$winner_elo <- NA
  data$loser_elo <- NA
  
  for (i in 1:nrow(data)) {
    winner <- data$winner_name[i]
    loser <- data$loser_name[i]
    
    # get current elo ratings
    winner_elo <- players_elo$elo[players_elo$players == winner]
    loser_elo <- players_elo$elo[players_elo$players == loser]
    
    # store pre-match elo in dataframe
    data$winner_elo[i] <- winner_elo
    data$loser_elo[i] <- loser_elo
    
    # calculate new elo ratings
    new_elo <- elo.calc(wins.A = 1,
                        elo.A = winner_elo,
                        elo.B = loser_elo,
                        k = k)
    
    # update elo ratings
    players_elo$elo[players_elo$players == winner] <- new_elo[1, 1]
    players_elo$elo[players_elo$players == loser] <- new_elo[1, 2]
  }
  
  return(data)
}




calculate_surface_elo <- function(data, k = 32) {
  
  # create starting elo ratings for every player
  players <- unique(c(data$winner_name, data$loser_name))
  empty_elo <- data.frame(players = players, elo = rep(1500, length(players)))
  
  # add id column to preserve match order after splitting by surface
  data$orig_id <- 1:nrow(data)
  
  # inner function to update elo ratings for a given surface
  update_surface_elo <- function(data, players_surf_elo, k = 32) {
    
    data$winner_surf_elo <- NA
    data$loser_surf_elo <- NA
    
    for (i in 1:nrow(data)) {
      winner <- data$winner_name[i]
      loser <- data$loser_name[i]
      
      winner_elo <- players_surf_elo$elo[players_surf_elo$players == winner]
      loser_elo <- players_surf_elo$elo[players_surf_elo$players == loser]
      
      data$winner_surf_elo[i] <- winner_elo
      data$loser_surf_elo[i] <- loser_elo
      
      new_elo <- elo.calc(wins.A = 1,
                          elo.A = winner_elo,
                          elo.B = loser_elo,
                          k = k)
      
      players_surf_elo$elo[players_surf_elo$players == winner] <- new_elo[1, 1]
      players_surf_elo$elo[players_surf_elo$players == loser] <- new_elo[1, 2]
    }
    
    return(list(match_results = data, final_rankings = players_surf_elo))
  }
  
  # inner function to process a single surface
  process_surface <- function(surf_name, full_data, starting_elo, k = 32) {
    
    sub_df <- full_data[full_data$surface == surf_name, ]
    results <- update_surface_elo(sub_df, starting_elo, k = k)
    res <- results$match_results
    
    colnames(res)[colnames(res) == "winner_surf_elo"] <- "winner_surface_elo"
    colnames(res)[colnames(res) == "loser_surf_elo"] <- "loser_surface_elo"
    
    return(res)
  }
  
  # calculate elo for each surface using fresh starting elos each time
  grass_results   <- process_surface("Grass",   data, empty_elo, k)
  clay_results    <- process_surface("Clay",     data, empty_elo, k)
  carpet_results  <- process_surface("Carpet",   data, empty_elo, k)
  hard_results    <- process_surface("Hard",     data, empty_elo, k)
  
  # recombine and restore original match order
  data <- rbind(grass_results, clay_results, carpet_results, hard_results)
  data <- data[order(data$orig_id), ]
  data$orig_id <- NULL
  
  return(data)
}



to_long_data <- function(data){

  long_data <- bind_rows( #bind rows stacks the two dataframes vertically
    
    #winner data
    data %>% transmute(match_id = row_number(), #transmute creates new df with column names and values
                       tourney_name, 
                       tourney_level,
                       date = tourney_date,
                       best_of,
                       round,
                       player_id = winner_id, 
                       player = winner_name,
                       player_hand = winner_hand,
                       player_height = winner_ht,
                       player_age = winner_age,
                       player_rank = winner_rank,
                       player_elo = winner_elo,
                       player_surf_elo = winner_surface_elo,
                       player_rank_points = winner_rank_points,
                       player_seed = winner_seed,
                       opponent_id = loser_id,
                       opponent = loser_name,
                       opponent_hand = loser_hand,
                       opponent_height = loser_ht,
                       opponent_age = loser_age,
                       opponent_rank = loser_rank,
                       opponent_elo = loser_elo,
                       opponent_surf_elo = loser_surface_elo,
                       opponent_rank_points = loser_rank_points,
                       opponent_seed = loser_seed,
                       surface,
                       indoor,
                       minutes, 
                       player_ace = w_ace, 
                       player_df = w_df,
                       player_svpt = w_svpt,
                       player_first_srv_in = w_1stIn,
                       player_first_srv_won = w_1stWon,
                       player_second_serv_won = w_2ndWon,
                       player_serve_gms = w_SvGms,
                       player_bp_saved = w_bpSaved,
                       player_bp_faced = w_bpFaced,
                       opponent_ace = l_ace, 
                       opponent_df = l_df,
                       opponent_svpt = l_svpt,
                       opponent_first_srv_in = l_1stIn,
                       opponent_first_srv_won = l_1stWon,
                       opponent_second_serv_won = l_2ndWon,
                       opponent_serve_gms = l_SvGms,
                       opponent_bp_saved = l_bpSaved,
                       opponent_bp_faced = l_bpFaced,
                       outcome = 1,),
    #loser data
    data %>% transmute(match_id = row_number(),
                       tourney_name, 
                       tourney_level,
                       date = tourney_date, 
                       best_of,
                       round,
                       player_id = loser_id, 
                       player = loser_name,
                       player_hand = loser_hand,
                       player_height = loser_ht,
                       player_age = loser_age,
                       player_rank = loser_rank,
                       player_elo = loser_elo,
                       player_surf_elo = loser_surface_elo,
                       player_rank_points = loser_rank_points,
                       player_seed = loser_seed,
                       opponent_id = winner_id,
                       opponent = winner_name,
                       opponent_hand = winner_hand,
                       opponent_height = winner_ht,
                       opponent_age = winner_age,
                       opponent_rank = winner_rank,
                       opponent_elo = winner_elo,
                       opponent_surf_elo = winner_surface_elo,
                       opponent_rank_points = winner_rank_points,
                       opponent_seed = winner_seed,
                       surface,
                       indoor,
                       minutes, 
                       player_ace = l_ace, 
                       player_df = l_df,
                       player_svpt = l_svpt,
                       player_first_srv_in = l_1stIn,
                       player_first_srv_won = l_1stWon,
                       player_second_serv_won = l_2ndWon,
                       player_serve_gms = l_SvGms,
                       player_bp_saved = l_bpSaved,
                       player_bp_faced = l_bpFaced,
                       opponent_ace = w_ace, 
                       opponent_df = w_df,
                       opponent_svpt = w_svpt,
                       opponent_first_srv_in = w_1stIn,
                       opponent_first_srv_won = w_1stWon,
                       opponent_second_serv_won = w_2ndWon,
                       opponent_serve_gms = w_SvGms,
                       opponent_bp_saved = w_bpSaved,
                       opponent_bp_faced = w_bpFaced,
                       outcome = 0,)
  ) %>% arrange(date)

  
  return(long_data)
  
  
}
  

extra_feats <- function(long_data){

  long_data <- long_data %>% mutate(
    #rank differences
    rank_diff = player_rank - opponent_rank,
    rank_diff_abs = abs(player_rank - opponent_rank),
    
    #age differences
    age_diff = player_age - opponent_age,
    age_diff_abs = abs(player_age - opponent_age),
    
    #height differences
    height_diff = player_height - opponent_height,
    height_diff_abs = abs(player_height - opponent_height),
    
    #elo differences
    elo_diff = player_elo - opponent_elo,
    elo_diff_abs = abs(player_elo - opponent_elo),
    
    #rank points differences
    rank_points_diff = player_rank_points - opponent_rank_points,
    rank_points_diff_abs = abs(player_rank_points - opponent_rank_points),
    
    #seed differences
    seed_diff = player_seed - opponent_seed,
    seed_abs = abs(player_seed - opponent_seed),
    
    
  )

  return(long_data)
}