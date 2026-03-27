import pandas as pd
import re
import numpy as np

data = pd.read_csv("tennis_extnd_feats_v1.csv")


data['date'] = pd.to_datetime(data['date'])



def head_to_head(df):

    df["h2h_wins"] = np.nan
    df["h2h_losses"] = np.nan
    df["h2h_total"] = np.nan
    df["h2h_win_prcnt"] = np.nan
    df['last_h2h_result'] = np.nan


    for i, match in df.iterrows():
        current_date = match['date']
        current_player = match['player']
        current_opp = match['opponent']


        #find all previous matches
        previous_meets = df.loc[(df['date'] < current_date) 
        & (((df['player'] == current_player) & (df['opponent'] == current_opp))
        | ((df['player'] == current_opp) & (df['opponent'] == current_player)))]

        
        unique_meetings = previous_meets.drop_duplicates(subset=["match_id"])


        if len(unique_meetings) > 0:
            wins = len(unique_meetings[
                ((unique_meetings['player'] == current_player) & (unique_meetings['outcome'] == 1)) |
                ((unique_meetings['opponent'] == current_player) & (unique_meetings['outcome'] == 0))
                ])
            

            total = len(unique_meetings)
            losses = total - wins

            df.loc[i, "h2h_wins"] = wins
            df.loc[i, "h2h_losses"] = losses
            df.loc[i, "h2h_total"] = total
            df.loc[i, "h2h_win_prcnt"] = wins/total if total > 0 else np.nan


            #last meeting
            last_meeting = unique_meetings.sort_values(by='date', ascending=False).iloc[0]

            if last_meeting['player'] == current_player:
                df.loc[i, 'last_h2h_result'] = last_meeting['outcome']
            else:
                df.loc[i, 'last_h2h_result'] = 1 - last_meeting['outcome']

        #no prev meetings
        else:
            df.loc[i, "h2h_wins"] = 0
            df.loc[i, "h2h_losses"] = 0
            df.loc[i, "h2h_total"] = 0
            df.loc[i, "h2h_win_prcnt"] = np.nan
            df.loc[i, 'last_h2h_result'] = np.nan 

        if i % 10000 == 0:
            print(f"Processed {i} rows")

    return df



h2h_data = head_to_head(data)


h2h_data.to_csv("h2h_crc.csv")