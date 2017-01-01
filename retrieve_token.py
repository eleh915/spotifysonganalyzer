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
import initial_auth

print('retrieve token script starting...')

# At this point, a browser will open and the user will need to enter the URL into the Shiny app
# Once the user enters in this info as an input, we can get the token, which we will use
# to get their data

# User will pass in URL as an argument
parser = argparse.ArgumentParser(description='Command line parameters')
parser.add_argument('-u', '--url', nargs=1, type=str, help="URL -- redirect URI + code used for generating auth token")
args = vars(parser.parse_args())
response = args['url'][0]
print('response: ')
print (response)

# Feeding URL response into sp_oauth function to get code
code = initial_auth.sp_oauth.parse_response_code(response)
print('code: ')
print(code)
token_info = initial_auth.sp_oauth.get_access_token(code)
print('token_info: ')
print(token_info)

# Auth'ed API request
if token_info:
    print('access token: ')
    print(token_info['access_token'])
    #return(token_info['access_token'])
#else:
#    return None
