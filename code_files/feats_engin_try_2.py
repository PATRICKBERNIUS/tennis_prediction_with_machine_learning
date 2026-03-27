import pandas as pd
import numpy as np
import re
import tqdm as tqdm


win = pd.read_csv("temp_for_crc.csv")


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

    for i, match in tqdm.tqdm(df.iterrows(), total=len(df), desc="Calculating time on court and rest time"): #for each match 
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

        player_mins = player_prev_rounds['minutes'].sum() #count all match minute in previous rounds for the player
        opp_mins = opp_prev_rounds['minutes'].sum() #count all match minute in previous rounds for the opponent

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
            player_days_rest = np.nan

        if pd.notna(opp_most_recent_match_date):
            opp_days_rest = (current_tourney_date - opp_most_recent_match_date).days
        else:
            opp_days_rest = np.nan

        # add to new columns
        df.loc[i, 'player_rest_time'] = player_days_rest
        df.loc[i, 'opponent_rest_time'] = opp_days_rest

    return df


time = time_on_court_and_rest(win)


time.to_csv("it_would_just_be_so_awesome_if_this_worked.csv")