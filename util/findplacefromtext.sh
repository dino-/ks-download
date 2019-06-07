#! /bin/bash

apiKey=$(<$HOME/.GoogleAPIKey)

# hq
lat=35.7879274
lng=-78.6506085

# fields="&fields=formatted_address,geometry,icon,id,name,permanently_closed,photos,place_id,plus_code,scope,types"
fields="&fields=place_id,name,formatted_address,geometry,types,permanently_closed"

url="https://maps.googleapis.com/maps/api/place/findplacefromtext/json?key=$apiKey&inputtype=textquery&input=taqueria%20la$fields&locationbias=circle:2000@35.82813429999999,-78.5848544&types=restaurant|cafe|bar"

curl "$url" | json_reformat
