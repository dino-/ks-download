#! /bin/bash

apiKey=$(<$HOME/.GoogleAPIKey)

placeId="ChIJOXDCRmpZrIkRwrvIwCZALHc"

fields="&fields=place_id,name,address_components,formatted_address,vicinity,geometry,types,permanently_closed"

url="https://maps.googleapis.com/maps/api/place/details/json?key=$apiKey&place_id=$placeId$fields"

curl "$url" | json_reformat
