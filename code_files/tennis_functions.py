import pandas as pd
import re
import numpy as np
from tqdm import tqdm


#head2head
def head_to_head(df):


    df = df.copy()

    #ensure data is sorted by date
    df = df.sort_values(['date']).reset_index(drop=True)

    #empty columns for our new features
    df["h2h_wins"] = np.nan
    df["h2h_losses"] = np.nan
    df["h2h_total"] = np.nan
    df["h2h_win_prcnt"] = np.nan
    df['last_h2h_result'] = np.nan


    for i, match in tqdm(df.iterrows(), total=len(df), desc="Calculating H2H stats"): #for each match
        current_date = match['date'] #grab current date
        current_player = match['player'] #current player
        current_opp = match['opponent'] #current opponent


        #find all previous matches
        previous_meets = df.loc[(df['date'] < current_date) #every match that took place on a previous date
        & (((df['player'] == current_player) & (df['opponent'] == current_opp)) #where current player plays the current opponent
        | ((df['player'] == current_opp) & (df['opponent'] == current_player)))] #or current opponent plays current player. These are the same match, but since the data is doubled, we need to select both instances.

        
        unique_meetings = previous_meets.drop_duplicates(subset=["match_id"]) #only select one of the instances for each match


        if len(unique_meetings) > 0: #if the players have played before
            wins = len(unique_meetings[
                ((unique_meetings['player'] == current_player) & (unique_meetings['outcome'] == 1)) |
                ((unique_meetings['opponent'] == current_player) & (unique_meetings['outcome'] == 0))
                ]) #a win is counted if the current player is in the player perspective and won the match, or if the current player is the oppononet and the match was lost from the player perspective
            

            total = len(unique_meetings) #total meetings between the two players
            losses = total - wins

            df.loc[i, "h2h_wins"] = wins
            df.loc[i, "h2h_losses"] = losses
            df.loc[i, "h2h_total"] = total
            df.loc[i, "h2h_win_prcnt"] = wins/total if total > 0 else np.nan


            #last meeting
            last_meeting = unique_meetings.sort_values(by='date', ascending=False).iloc[0]

            #finds winner of last meeting
            if last_meeting['player'] == current_player: #if from player perspective
                df.loc[i, 'last_h2h_result'] = last_meeting['outcome'] #take the outcome
            else: #if from opponent perspective, flip the outcome. Win for opponent means loss for player
                df.loc[i, 'last_h2h_result'] = 1 - last_meeting['outcome'] 

        #no prev meetings
        else:
            df.loc[i, "h2h_wins"] = 0
            df.loc[i, "h2h_losses"] = 0
            df.loc[i, "h2h_total"] = 0
            df.loc[i, "h2h_win_prcnt"] = np.nan
            df.loc[i, 'last_h2h_result'] = np.nan 


    return df





#Dummies for surfaces and Weighted ELOs


def surface_and_elo(df):
    df = df.copy()

    df = pd.get_dummies(df, columns=['surface'], prefix='surf', dtype=int, drop_first=True)
    #taking the average of the two
    df['player_averaged_elo'] = (df['player_elo'] + df['player_surf_elo']) / 2
    df['opponent_averaged_elo'] = (df['opponent_elo'] + df['opponent_surf_elo']) / 2

    #weighting the two, with more importance on overall elo
    df['player_weighted_elo'] = (df['player_elo'] * 0.6) + (df['player_surf_elo'] * 0.4)
    df['opponent_weighted_elo'] = (df['opponent_elo'] * 0.6) + (df['opponent_surf_elo'] * 0.4)

    return df


#win streaks
def streaks(df):


    df = df.copy()

    #maps rounds to numbers so we can sort them easier
    ROUND_ORDER = {
        'RR':     1,
        'R128':   2,
        'R64':    3,
        'R32':    4,
        'R16':    5,
        'QF':     6,
        'SF':     7,
        'BR':     8,
        '3rd/4th': 8,
        'F':      9
    }

    

    if "round_order" not in df.columns:
        df['round_order'] = df['round'].map(ROUND_ORDER) #new column with numeric vales for round

    df = df.sort_values(['date', 'round_order'])
        
    #df['round_order'] = df['round'].map(ROUND_ORDER) #new column with numeric vales for round
    df['player_win_loss_streak'] = np.nan #empty player streak column
    df['opponent_win_loss_streak'] = np.nan #empty opponent streak column

    for i, match in tqdm(df.iterrows(), total=len(df), desc="Calculating win streaks"): #for each match
        current_date = match['date'] #store the date
        current_player = match['player'] #player
        current_opp = match['opponent'] #opponent
        current_tourney = match['tourney_name'] #tournament name
        current_round = match['round_order'] #round

        

        past_tourney = df['date'] < current_date #all previous matches

        #since we only have tournament date, this tracks matches within the same tourney
        same_tourney_diff_match = ( #select matches if
            (df['date'] == current_date) & #same date
            (df['tourney_name'] == current_tourney) & #same tournament name
            (df['round_order'] < current_round) #earlier round
        )

        for person, col in [(current_player, 'player_win_loss_streak'), (current_opp, 'opponent_win_loss_streak')]: #for each match, find the streaks for the player and opponent

            playa = (df['player'] == person) | (df['opponent'] == person) #selects row where current player is player or opponent
            #past matches to look at where its the same player and either a previous tournament or previous round
            past_matches = df.loc[playa & (past_tourney | same_tourney_diff_match)]

            #reverses match order to most recent to oldest, removing duplicates
            backward_matches = past_matches.sort_values(by=['date', 'round_order'], ascending=False).drop_duplicates(subset=['match_id'])

            streak = 0 #streak counter
            streak_type = None #keeps track of win/loss

            for j, m in backward_matches.iterrows(): #for each match in the previous matches
                
                if m['player'] == person: #if the player of the match is our current player
                    player_won = m['outcome'] == 1 #they won the match if the outcome is 1
                else: #if they are the opponent
                    player_won = m['outcome'] == 0 #they won the match if the outcome is 0


                if streak_type is None:
                    streak_type = 'win' if player_won else 'loss' #update streak to win if they won, otherwise update it as loss

                current_result = 'win' if player_won else 'loss' #keep track of the most recent result


                if streak_type == current_result: #if the current result matches the streak type
                    streak += 1 # add to the streak counter
                else:
                    break #break when the streak ends

            if streak_type is None:
                df.loc[i, col] = 0 #zero if no previous matches
            elif streak_type == 'win':
                df.loc[i, col] = streak #positive number if it's a win streak
            else: 
                df.loc[i, col] = -streak #negative number if it's a losing streak


    return df



def more_cat_vars(df):
    df = df.copy()

    #dummy variables for tournament level, round, handedness, and if the match is indoors
    df = pd.get_dummies(df, columns=['tourney_level', 'round', 'player_hand', 'indoor'], prefix=['tourn_', 'round_', 'hand_', 'in_'], dtype=int, drop_first=True)

    df['date'] = pd.to_datetime(df['date']) #ensuring date dtype
    df['year'] = df['date'].dt.year #column for year
    df['month'] = df['date'].dt.month #column for month

    return df







#match stats
def calc_match_stats(df):
    df = df.copy()

    df = df.sort_values(['date', 'round_order'])

    #stats from player perspective
    player_stats = [
        'player_ace', 
        'player_df',
        'player_svpt',
        'player_first_srv_in',
        'player_first_srv_won',
        'player_second_serv_won',
        'player_serve_gms',
        'player_bp_saved',
        'player_bp_faced'
    ]  
    #stats from opponent perspective
    opp_stats = [
        'opponent_ace', 
        'opponent_df',
        'opponent_svpt',
        'opponent_first_srv_in',
        'opponent_first_srv_won',
        'opponent_second_serv_won',
        'opponent_serve_gms',
        'opponent_bp_saved',
        'opponent_bp_faced'
    ]

    
    #rolling windows to calculate stats
    windows = [1, 3, 5, 10, 30, 60, 90]

    new_cols = {}

    #handles player stats
    for stat in player_stats: #for each stat
        for window in windows: #for each window
            new_cols[f'{stat}_rate_window_{window}'] = ( #create a new column of our stat over that window
                df.groupby('player_id')[stat] #groups by player
                .transform(lambda x, w=window: x.shift(1).rolling(window=w, min_periods=(max(1, w // 2))).mean())
                #transform applys a function to our dataframe. This function shifts the dataframe values one row forward, so we only calculate previous matches. Creates a rolling window with the specified size. The minimum number of matches either 1 or the window size divided by two rounded down. This assures we don't have two many empty values for matches early in a player's career
            )

    #handles opponent stats
    for stat in opp_stats: #for each stat
        for window in windows: #for each window
            new_cols[f'{stat}_rate_window_{window}'] = ( #create a new column of our stat over that window
                df.groupby('opponent_id')[stat] #groups by player
                .transform(lambda x, w=window: x.shift(1).rolling(window=w, min_periods=(max(1, w // 2))).mean())
                #transform applys a function to our dataframe. This function shifts the dataframe values one row forward, so we only calculate previous matches. Creates a rolling window with the specified size. The minimum number of matches either 1 or the window size divided by two rounded down. This assures we don't have two many empty values for matches early in a player's career
            )

    #combines our current dataframe horizontally, with a dataframe created of our new columns with the same index
    df = pd.concat([df, pd.DataFrame(new_cols, index=df.index)], axis=1)

    df = df.copy()


    #dropping stats within match to prevent leakage
    df = df.drop(columns=player_stats)
    df = df.drop(columns=opp_stats)

    return df




#win rates across windows
def win_rates(df):
    df = df.copy()

    df['date'] = pd.to_datetime(df['date'])

    df = df.sort_values(['date', 'round_order'])

    #day windows. the "D" indicates to pandas that it is days not rows
    windows = ['30D', '60D', '90D']

    #ensure matches are sorted by date and date is set as the index, so we can use rolling window functions on the date
    df = df.sort_values('date').set_index('date')


    for window in tqdm(windows, desc="Calculating rolling window stats"): #for each window
        #playerperspective
        #matches played
        df[f"player_matches_played_last_{window}ays"] = ( #create a new column of matches player 
            df.groupby('player_id')['outcome'] #group by player id
            .transform(lambda x: x.shift(1).rolling(window).count())
            #shift the dataframe by one to avoid leakage, compute rolling window, count the number of matches within that window
        )
        
        #win rate
        df[f"player_win_rate_last_{window}ays"] = ( #create a new column of matches player  
            df.groupby('player_id')['outcome'] #group by player id
            .transform(lambda x: x.shift(1).rolling(window).mean()) 
            #shift the dataframe by one to avoid leakage, compute rolling window, count the number of matches within that window
        )

        #opponent perspective
        #matches played
        df[f"opponent_matches_played_last_{window}ays"] = ( #create a new column of matches player 
            df.groupby('opponent_id')['outcome'] #group by player id
            .transform(lambda x: x.shift(1).rolling(window).count())
            #shift the dataframe by one to avoid leakage, compute rolling window, count the number of matches within that window
        )
        
        #win rate
        df[f"opponent_win_rate_last_{window}ays"] = ( #create a new column of matches player  
            df.groupby('opponent_id')['outcome'] #group by player id
            .transform(lambda x: x.shift(1).rolling(window).mean()) 
            #shift the dataframe by one to avoid leakage, compute rolling window, count the number of matches within that window
        
        )
        

    df = df.reset_index()

    return df





#time on court and rest time
def time_on_court_and_rest(df):

    df = df.copy()

    #ensure date dtpye
    df['date'] = pd.to_datetime(df['date'])
    df = df.sort_values(['date', 'round_order'])

    #create new columns
    df['player_time_on_court_current_tourney'] = np.nan #player time on court
    df['opponent_time_on_court_current_tourney'] = np.nan #opponent time on court
    df['player_rest_time'] = np.nan #player rest
    df['opponent_rest_time'] = np.nan #opponenet rest

    for i, match in tqdm(df.iterrows(), total=len(df), desc="Calculating time on court and rest time"): #for each match 
        current_tourney_date = match['date'] #grab current date
        current_player = match['player_id'] #current player
        current_opp = match['opponent_id'] #opponent 
        current_tourney = match['tourney_name'] #tournament 
        current_round = match['round_order'] #round

        #player perspective
        player_prev_rounds = df.loc[(df['date'] == current_tourney_date) & #same tournement date
                            (df['player_id'] == current_player) & #same player
                            (df['tourney_name'] == current_tourney) & #same tournament
                            (df['round_order'] < current_round)].drop_duplicates(subset=['match_id']) #previous round, drop duplicates

        #opponent perspective
        opp_prev_rounds = df.loc[(df['date'] == current_tourney_date) & #same tournement date
                            (df['player_id'] == current_opp) & #same opponent
                            (df['tourney_name'] == current_tourney) & #previous round, drop duplicates
                            (df['round_order'] < current_round)].drop_duplicates(subset=['match_id']) #previous round, drop duplicates

        player_mins = sum(player_prev_rounds['minutes']) #count all match minute in previous rounds for the player
        opp_mins = sum(opp_prev_rounds['minutes']) #count all match minute in previous rounds for the opponent

        # add to new columns
        df.loc[i, 'player_time_on_court_current_tourney'] = player_mins
        df.loc[i, 'opponent_time_on_court_current_tourney'] = opp_mins


        #rest
        player_prior_matches = df.loc[(df['date'] < current_tourney_date) & (df['player_id'] == current_player)] #player's previous matches
        opp_prior_matches = df.loc[(df['date'] < current_tourney_date) & (df['player_id'] == current_opp)] #opponent's previous matches

        player_most_recent_match_date = player_prior_matches['date'].max() #most tournament for player
        opp_most_recent_match_date = opp_prior_matches['date'].max() #most recent tournament for opponent

        #compute days between current tournament and last tournament
        if pd.notna(player_most_recent_match_date):
            player_days_rest = (current_tourney_date - player_most_recent_match_date).days
        else:
            player_day_rest = np.nan

        if pd.notna(opp_most_recent_match_date):
            opp_days_rest = (current_tourney_date - opp_most_recent_match_date).days
        else:
            opp_days_rest = np.nan

        # add to new columns
        df.loc[i, 'player_rest_time'] = player_days_rest
        df.loc[i, 'opponent_rest_time'] = opp_days_rest

    return df




def win_rates_v2(df):
    df = df.copy()
    df['date'] = pd.to_datetime(df['date'])
    
    # Save original order
    df['_original_order'] = range(len(df))
    
    # Sort chronologically
    df = df.sort_values(['date', 'round_order'])
    
    # Match-count windows for win rate
    match_windows = [5, 10, 20, 30]
    
    # Time windows for activity level
    time_windows = ['30D', '60D', '90D']
    
    # Calculate match-based win rates
    for window in tqdm(match_windows, desc="Calculating match-based win rates"):
        df[f"win_rate_last_{window}_matches"] = (
            df.groupby('player_id')['outcome']
            .transform(lambda x: x.shift(1).rolling(window=window, min_periods=max(1, window // 2)).mean())
        )
    
    # Calculate time-based activity
    df = df.set_index('date')
    for window in tqdm(time_windows, desc="Calculating time-based activity"):
        df[f"matches_played_last_{window}"] = (
            df.groupby('player_id')['outcome']
            .transform(lambda x: x.shift(1).rolling(window).count())
        )
    df = df.reset_index()
    
    # Map to player and opponent columns
    for window in match_windows:
        df[f'player_win_rate_last_{window}_matches'] = df[f'win_rate_last_{window}_matches']
        
        # FIXED: Include round_order to avoid duplicate keys
        winrate_dict = df.set_index(['player_id', 'date', 'round_order'])[f'win_rate_last_{window}_matches'].to_dict()
        df[f'opponent_win_rate_last_{window}_matches'] = [
            winrate_dict.get((opp, d, r), np.nan)
            for opp, d, r in zip(df['opponent_id'], df['date'], df['round_order'])
        ]
        df = df.drop(columns=[f'win_rate_last_{window}_matches'])
    
    for window in time_windows:
        df[f'player_matches_played_last_{window}'] = df[f'matches_played_last_{window}']
        
        # FIXED: Include round_order
        matches_dict = df.set_index(['player_id', 'date', 'round_order'])[f'matches_played_last_{window}'].to_dict()
        df[f'opponent_matches_played_last_{window}'] = [
            matches_dict.get((opp, d, r), np.nan)
            for opp, d, r in zip(df['opponent_id'], df['date'], df['round_order'])
        ]
        df = df.drop(columns=[f'matches_played_last_{window}'])
    
    # Restore original order
    df = df.sort_values('_original_order').drop(columns=['_original_order'])
    
    return df








def calc_match_stats(df):
    df = df.copy()
    
    # Sort by player first, then date/round
    df = df.sort_values(['date', 'round_order'])
    
    # Save original order
    df['_original_order'] = range(len(df))
    
    player_stats = [
        'player_ace', 
        'player_df',
        'player_svpt',
        'player_first_srv_in',
        'player_first_srv_won',
        'player_second_serv_won',
        'player_serve_gms',
        'player_bp_saved',
        'player_bp_faced'
    ]  
    
    opp_stats = [
        'opponent_ace', 
        'opponent_df',
        'opponent_svpt',
        'opponent_first_srv_in',
        'opponent_first_srv_won',
        'opponent_second_serv_won',
        'opponent_serve_gms',
        'opponent_bp_saved',
        'opponent_bp_faced'
    ]
    
    windows = [1, 3, 5, 10, 30, 60, 90]
    
    # Calculate for everyone as PLAYER
    for stat in player_stats:
        for window in windows:
            df[f'{stat}_rate_window_{window}'] = (
                df.groupby('player_id')[stat]
                .transform(lambda x, w=window: x.shift(1).rolling(window=w, min_periods=max(1, w // 2)).mean())
            )
    
    # Map to opponent using lookup (THIS IS THE KEY PART)
    for player_stat, opp_stat in zip(player_stats, opp_stats):
        for window in windows:
            stat_dict = df.set_index(['player_id', 'date', 'round_order'])[f'{player_stat}_rate_window_{window}'].to_dict()
            
            df[f'{opp_stat}_rate_window_{window}'] = [
                stat_dict.get((opp, d, r), np.nan)
                for opp, d, r in zip(df['opponent_id'], df['date'], df['round_order'])
            ]
    
    df = df.sort_values('_original_order').drop(columns='_original_order')
    df = df.drop(columns=player_stats + opp_stats)
    
    return df