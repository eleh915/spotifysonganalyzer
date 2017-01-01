from __future__ import print_function
import os
from spotipy import oauth2
import sys
import spotipy
import pandas as pd
import json
from datetime import date,timedelta,datetime
#import gracenote_api
import requests
import lxml.etree as etree
import itertools
import webbrowser
import argparse

# User will pass in username as an argument
parser = argparse.ArgumentParser(description='Command line parameters')
parser.add_argument('-u', '--username', nargs=1, type=str, help="Your Spotify username, e.g. 122681512 or lehworthing")
args = vars(parser.parse_args())
username = args['username'][0]

config = json.load(open('/Users/eleh/spotify/spotifysonganalyzer/api_config.json'))

SPOTIPY_CLIENT_ID = client_id = config['spotify']['client_id']
SPOTIPY_CLIENT_SECRET = client_secret = config['spotify']['client_secret']
SPOTIPY_REDIRECT_URI = redirect_uri = 'https://eleh915.shinyapps.io/spotifysonganalyzer/'

scope = 'user-library-read'
#username = '122681512'

sp_oauth = oauth2.SpotifyOAuth(client_id, client_secret, redirect_uri, 
        scope=scope, cache_path=".cache-" + username )
print('sp_oauth: ')
print(sp_oauth)

token_info = sp_oauth.get_cached_token()
print(token_info)

# If there is no cached token, get one
#if token_info:
if not token_info:
    auth_url = sp_oauth.get_authorize_url()
    try:
        webbrowser.open(auth_url)
        print("Opened %s in your browser" % auth_url)
    except:
        print("Please navigate here: %s" % auth_url)

# At this point, a browser will open and the user will need to enter the URL into the Shiny app
# Once the user enters in this info as an input, we can get the token, which we will use
# to get their data
