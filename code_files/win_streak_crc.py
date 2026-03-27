import pandas as pd
import re
import numpy as np

ROUND_ORDER = {
    'RR':     1,
    'R256':   2,
    'R128':   3,
    'R64':    4,
    'R32':    5,
    'R16':    6,
    'QF':     7,
    'SF':     8,
    'BR':     9,
    '3rd/4th': 9,
    'F':      10,
    'Fs':     10,
}

def streaks(df):



    df = df.copy()
    df['round_order'] = df['round'].map(ROUND_ORDER)
    df['win_loss_streak'] = np.nan

    for i, match in df.iterrows():
        current_date = match['date']
        current_player = match['player']
        current_tourney = match['tourney_name']
        current_round = match['round_order']

        playa = (df['player'] == current_player) | (df['opponent'] == current_player)

        past_tourney = df['date'] < current_date

        same_tourney_diff_match = (
            (df['date'] == current_date) &
            (df['tourney_name'] == current_tourney) &
            (df['round_order'] < current_round)
        )

        past_matches = df.loc[playa & (past_tourney | same_tourney_diff_match)]

        backward_matches = past_matches.sort_values(by=['date', 'round_order'], ascending=False).drop_duplicates(subset=['match_id'])

        streak = 0
        streak_type = None

        for j, m in backward_matches.iterrows():
            
            if m['player'] == current_player:
                player_won = m['outcome'] == 1
            else:
                player_won = m['outcome'] == 0


            if streak_type is None:
                streak_type = 'win' if player_won else 'loss'

            current_result = 'win' if player_won else 'loss'


            if streak_type == current_result:
                streak += 1
            else:
                break

        if streak_type is None:
            df.loc[i, 'win_loss_streak'] = 0
        elif streak_type == 'win':
            df.loc[i, 'win_loss_streak'] = streak
        else: 
            df.loc[i, 'win_loss_streak'] = -streak



        

    return df




data = pd.read_csv("h2h_data_surface_dummies.csv")


new_dat = streaks(data)

new_dat.to_csv("win_streak_data.csv")