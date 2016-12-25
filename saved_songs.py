import sys
import spotipy
import spotipy.util as util
import pandas as pd
import json
from datetime import date,timedelta,datetime

import gracenote_api
import json
import requests
import lxml.etree as etree
import itertools

config = json.load(open('api_config.json'))

SPOTIPY_CLIENT_ID = client_id = config['spotify']['client_id']
SPOTIPY_CLIENT_SECRET = client_secret = config['spotify']['client_secret']
SPOTIPY_REDIRECT_URI = redirect_uri = 'https://bitbucket.org/atandy/spotify_api'

scope = 'user-library-read'
username = '122681512'

#token = util.prompt_for_user_token(username, scope)
token = util.prompt_for_user_token(username, scope, client_id, client_secret, redirect_uri)
sp = spotipy.Spotify(auth=token)

def get_saved_songs():

    saved_songs_df = pd.DataFrame()
    offset_val = 0
    stop_loop = False
    while (stop_loop is False):

        results = sp.current_user_saved_tracks(limit=50, offset=offset_val)

        if not results['items']:
            stop_loop=True

        for item in results['items']:
            track = item['track']
            track_id = track['id']
            audio_feats = sp.audio_features([track_id])

            name = track['name']
            artist = track['artists'][0]['name']
            track_img = track['album']['images'][0]['url']
            print 'track_img: ', track_img
            popularity = track['popularity']
            added_at = item['added_at'].split('T')[0]
            acousticness = audio_feats[0]['acousticness']
            danceability = audio_feats[0]['danceability']
            duration_ms = audio_feats[0]['duration_ms']
            energy = audio_feats[0]['energy']
            instrumentalness = audio_feats[0]['instrumentalness']
            key = audio_feats[0]['key']
            liveness = audio_feats[0]['liveness']
            loudness = audio_feats[0]['loudness']
            speechiness = audio_feats[0]['speechiness']
            tempo = audio_feats[0]['tempo']
            time_signature = audio_feats[0]['time_signature']
            valence = audio_feats[0]['valence']
            preview_url = track['preview_url']
            print 'preview_url: ', preview_url

            mini_df = pd.DataFrame({'name': name, 'artist': artist, 'popularity': popularity, 'acousticness': acousticness,
            'danceability': danceability, 'duration_ms': duration_ms, 'energy': energy, 'instrumentalness': instrumentalness,
            'key': key, 'liveness': liveness, 'loudness': loudness, 'speechiness': speechiness, 'tempo': tempo,
            'time_signature': time_signature, 'valence': valence, 'added_at': added_at, 'preview_url': preview_url,
            'track_img': track_img}, index=[0])
            print 'mini df: ', mini_df
            saved_songs_df = saved_songs_df.append(mini_df)

        offset_val += 50

    print saved_songs_df
    saved_songs_df.to_csv('df_saved_songs.csv', encoding='utf-8', index=False)

def remove_duplicates(orig_list):
    new_list = []
    for i in orig_list:
         if i not in new_list:
            new_list.append(i)
    return new_list

# Function to pull mood data from Gracenote API
def get_data(saved_songs_df, row):
    song = saved_songs_df.name.iloc[row]
    artist = saved_songs_df.artist.iloc[row]
    added_at = saved_songs_df.added_at.iloc[row]
    r = g.get_track(song, artist)
    moods = r['moods']
    moods = remove_duplicates(moods)
    return [song, artist, added_at, moods]

g = gracenote_api.Gracenote()
g.register()

#saved_songs_df = get_saved_songs()
saved_songs_df = pd.read_csv('df_saved_songs.csv')

# For each month, create count of moods
saved_songs_df['added_at'] = pd.to_datetime(saved_songs_df['added_at'], format='%Y-%m-%d')
saved_songs_df['added_at'] = saved_songs_df.added_at.map(lambda x: x.strftime('%Y-%m'))

date_mood_df = pd.DataFrame()
for row in range(0, len(saved_songs_df)):

    # Get data from Gracenote    
    song, artist, added_at, moods = get_data(saved_songs_df, row)
    for el in moods:
        if ' / ' in el:
            el1 = el.split(' / ')[0]
            el2 = el.split(' / ')[1]
            row = pd.DataFrame({el1:[1],el2:[1],'added_at':[added_at]})
        else:
            row = pd.DataFrame({el:[1], 'added_at':[added_at]})
        date_mood_df = pd.concat([date_mood_df,row], axis=0)
        print 'appended: \n', date_mood_df

# Get moods per month
month_df = date_mood_df.groupby(['added_at']).sum().reset_index()
#month_df['total_moods'] = date_mood_df.sum(axis=1).fillna(0)
if 'Unnamed: 0' in month_df:
    del month_df['Unnamed: 0']
print 'month_df: \n', month_df
month_df.to_csv('month_df.csv', index=False)

# Print some stats about top moods per month
month_df["moods_per_month"] = month_df.sum(axis=1)
pct_month_df = month_df.loc[:,"Abstract Beat":"Yearning"] = month_df.loc[:,"Abstract Beat":"Yearning"].div(month_df["moods_per_month"], axis=0).fillna(0) 
print 'pct_month_df: \n', pct_month_df
pct_month_df['top_mood_by_pct'] = pct_month_df.idxmax(axis=1)
pct_month_df = pct_month_df.merge(month_df, how='left')
print 'top_mood_df: \n', pct_month_df[['added_at','top_mood_by_pct']]
