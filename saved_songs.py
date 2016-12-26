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
import argparse

config = json.load(open('api_config.json'))

SPOTIPY_CLIENT_ID = client_id = config['spotify']['client_id']
SPOTIPY_CLIENT_SECRET = client_secret = config['spotify']['client_secret']
SPOTIPY_REDIRECT_URI = redirect_uri = 'https://bitbucket.org/atandy/spotify_api'

scope = 'user-library-read'
username = '122681512'

# Read in args -- if user wants to use CSV rather than querying Spotify directly
parser = argparse.ArgumentParser(description='Command line parameters')
parser.add_argument('-f', '--file', nargs=1, type=str, help="the file you want to upload")
args = vars(parser.parse_args())

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
    return saved_songs_df

def remove_duplicates(orig_list):
    new_list = []
    for i in orig_list:
         if i not in new_list:
            new_list.append(i)
    return new_list

# Function to pull mood data from Gracenote API
def get_data(saved_songs_df, row, moods_or_genres):
    song = saved_songs_df.name.iloc[row]
    artist = saved_songs_df.artist.iloc[row]
    added_at = saved_songs_df.added_at.iloc[row]
    r = g.get_track(song, artist)
    moods_or_genres_list = r[str(moods_or_genres)]
    print 'moods_or_genres_list: ', moods_or_genres_list
    moods_or_genres_list = remove_duplicates(moods_or_genres_list)
    return [song, artist, added_at, moods_or_genres_list]

def get_mood_genre_data(saved_songs_df, moods_or_genres):

    full_df = pd.DataFrame()
    for row in range(0, len(saved_songs_df)):

        # Get data from Gracenote
        song, artist, added_at, m_ds = get_data(saved_songs_df, row, moods_or_genres)
        for el in m_ds:
            if ' / ' in el:
                el1 = el.split(' / ')[0]
                el2 = el.split(' / ')[1]
                row = pd.DataFrame({el1:[1],el2:[1],'added_at':[added_at]})
            else:
                row = pd.DataFrame({el:[1], 'added_at':[added_at]})
            full_df = pd.concat([full_df,row], axis=0)
        print 'appended: \n', full_df

    # Get moods/genres per month
    month_df = full_df.groupby(['added_at']).sum().reset_index()
    month_df.to_csv('month_df.csv', encoding='utf-8', index=False)
    return month_df

g = gracenote_api.Gracenote()
g.register()

# If there is a csv to use (and we don't wanna query each time), use it
if len(sys.argv) > 1:
    print 'User has passed a file'
    file = args['file'][0]
    full_df = pd.read_csv(file)
else:
    print 'We will query the Spotify API'
    full_df = get_saved_songs()

# Reading to csv and reingesting to convert to utf-8
full_df.to_csv('df_saved_songs.csv', encoding='utf-8', index=False)
saved_songs_df = pd.read_csv('df_saved_songs.csv')

# Map days to months
saved_songs_df['added_at'] = pd.to_datetime(saved_songs_df['added_at'], format='%Y-%m-%d')
saved_songs_df['added_at'] = saved_songs_df.added_at.map(lambda x: x.strftime('%Y-%m'))

# Get mood and genre data on a month-by-month basis
month_mood_df = get_mood_genre_data(saved_songs_df, 'moods')
month_genre_df = get_mood_genre_data(saved_songs_df, 'genres')
month_mood_df.to_csv('month_mood_df.csv', encoding='utf-8', index=False)
month_genre_df.to_csv('month_genre_df.csv', encoding='utf-8', index=False)

# Do we want to group genres into larger buckets? (e.g. Pop, Hip Hop, Rock, etc.)