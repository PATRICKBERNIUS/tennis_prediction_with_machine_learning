import pandas as pd
import re
import numpy as np


def time_on_court_and_rest(df):

    df = df.copy()

    df['date'] = pd.to_datetime(df['date'])

    df['player_time_on_court_current_tourney'] = np.nan
    df['opponent_time_on_court_current_tourney'] = np.nan
    df['player_rest_time'] = np.nan
    df['opponent_rest_time'] = np.nan

    for i, match in df.iterrows():
        current_tourney_date = match['date']
        current_player = match['player_id']
        current_opp = match['opponent_id']
        current_tourney = match['tourney_name']
        current_round = match['round_order']

        player_prev_rounds = df.loc[(df['date'] == current_tourney_date) & 
                            (df['player_id'] == current_player) & 
                            (df['tourney_name'] == current_tourney) &
                            (df['round_order'] < current_round)].drop_duplicates(subset=['match_id'])

        opp_prev_rounds = df.loc[(df['date'] == current_tourney_date) & 
                            (df['player_id'] == current_opp) & 
                            (df['tourney_name'] == current_tourney) &
                            (df['round_order'] < current_round)].drop_duplicates(subset=['match_id'])

        player_mins = sum(player_prev_rounds['minutes'])
        opp_mins = sum(opp_prev_rounds['minutes'])

        df.loc[i, 'player_time_on_court_current_tourney'] = player_mins
        df.loc[i, 'opponent_time_on_court_current_tourney'] = opp_mins


        #rest

        player_prior_matches = df.loc[(df['date'] < current_tourney_date) & (df['player_id'] == current_player)]
        opp_prior_matches = df.loc[(df['date'] < current_tourney_date) & (df['player_id'] == current_opp)]

        player_most_recent_match_date = player_prior_matches['date'].max()
        opp_most_recent_match_date = opp_prior_matches['date'].max()

        player_days_rest = (current_tourney_date - player_most_recent_match_date).days
        opp_days_rest = (current_tourney_date - opp_most_recent_match_date).days

        df.loc[i, 'player_rest_time'] = player_days_rest
        df.loc[i, 'opponent_rest_time'] = opp_days_rest

    return df


run = pd.read_csv("dat_dum_for_time_and_rest.csv")

dat = time_on_court_and_rest(run)


dat.to_csv("tennis_data_with_time.csv")