import requests
import json
import lxml.etree as etree

config = json.load(open('/Users/eleh/spotify/spotifysonganalyzer/api_config.json'))['gracenote']

# This is Tandy's code -- I take no credit
class Gracenote:
    def __init__(self):
        for key, value in config.items():
            setattr(self, key, value)
        self.headers = {'Content-Type': 'application/xml'}
        self.api_url = 'https://c{}.web.cddbp.net/webapi/xml/1.0/'.format(self.client_id)
        return 

    def _get(self, payload=None):
        try:
            return requests.get(self.api_url, headers=self.headers, data=payload)
        except Exception as e:
            print e
            return

    def _post(self, payload):
        try:
            return requests.post(self.api_url, data=payload)
        except Exception as e:
            print e
            return

    def register(self):
        register_query = """
        <QUERIES>
          <QUERY CMD="REGISTER">
            <CLIENT>{}</CLIENT>
          </QUERY>
        </QUERIES>""".format(self.client_id)
        resp = self._post(payload=register_query)
        tree = etree.fromstring(resp.content)
        for i in tree.iter(tag='USER'):
            self.user_id = i.text

    
    def get_track(self, artist, track_title):
        #print etree.tostring(tree, pretty_print=True)
        #parse_tags = ['MOOD', 'GENRE']
        query = """
        <QUERIES>
          <AUTH>
            <CLIENT>{client_id}</CLIENT>
            <USER>{user_id}</USER>
          </AUTH>
          <QUERY CMD="ALBUM_SEARCH">
            <MODE>SINGLE_BEST_COVER</MODE>
            <TEXT TYPE="ARTIST">{artist}</TEXT>
            <TEXT TYPE="TRACK_TITLE">{track_title}</TEXT>
            <OPTION>
              <PARAMETER>SELECT_EXTENDED</PARAMETER>
              <VALUE>COVER,REVIEW,ARTIST_BIOGRAPHY,ARTIST_IMAGE,ARTIST_OET,MOOD,TEMPO</VALUE>
            </OPTION>
            <OPTION>
              <PARAMETER>SELECT_DETAIL</PARAMETER>
              <VALUE>GENRE:3LEVEL,MOOD:2LEVEL,TEMPO:3LEVEL,ARTIST_ORIGIN:4LEVEL,ARTIST_ERA:2LEVEL,ARTIST_TYPE:2LEVEL</VALUE>
            </OPTION>
          </QUERY>
        </QUERIES>
        """.format(
            client_id=self.client_id, 
            user_id=self.user_id,
            artist=artist,
            track_title=track_title)

        try:
            response = self._post(payload=query)
            return self.parse_mood_genre(response.content)
        except Exception as e:
            print e
            return {'moods': [], 'genres': []}
       
    #TODO: this is ugly. 
    def parse_mood_genre(self, track_xml):
        moods = []
        genres = [] 
        try:
            tree = etree.fromstring(track_xml)
            for tag in tree.iter('MOOD'):
                moods.append(tag.text)
            for tag in tree.iter('GENRE'):
                genres.append(tag.text)
        except Exception as e:
            print e
        
        return {'moods': moods, 'genres': genres}
