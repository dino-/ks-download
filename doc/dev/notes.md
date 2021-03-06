# ks-download development notes

## Google Place Search API notes (2019-06-06)

service                     API call                          cost    free/month (max for $200 credit)
Find Place                  maps/api/place/findplacefromtext  0.017   11000
Place Search, Nearby Search maps/api/place/nearbysearch       0.032    5000
Place Search, Text Search   maps/api/place/textsearch         0.032    5000
Geocoding                                                     0.005   40000

What this means is, if we ONLY used Find Place, and NOTHING ELSE, we'd be able to do 11000 calls per month and have it covered by the $200. So, our overall usage has to be a combination of some amount of these. Also, Place Details is not on this chart yet, but we have a very small amount of those, we think.


## Importing the data into Mongo

- The example Mongo introduction stuff is suggesting that we may want to store one document for an entire place, and have all of the inspections in that document.
   - This has various implications but the biggest one is that the generated _id will not work any longer.
   - The existing _id generation was partly chosen to be workable _before_ the Google Places match


Trying to find all of the legacy data (what did you do?)

on tiddly.honuapps.com
   ~dino/ks/imported/
      nc-wake_2014
         nc-wake_2014/           3000     2014-01-24 - 2015-01-23
         nc-wake_2015-02-09/      157     2015-02-24 - 2015-02-09
         nc-wake_daily/
            nc-wake_2015-02-10/     8
            nc-wake_2015-02-11/    15
            nc-wake_2015-02-12/    17
         newOne/
   /var/local/kitchensnitch/nc-wake_daily/
      [all dirs]                 1352     2015-02-13 - present

Everything from imported/ above in one dir. This represents all
data before we automated daily downloading

   ~dino/ks/nc-wake_2014-01-24_2015-02-12/   3179

New mongodb server is up-to-date through 2015-07-09. Not yet automated for dailies after that date.

For purposes of populating a new db, everything we were interested in up to 2015-02-12 is in this file:

    data/nc-wake_2014-01-24_2015-02-12.tgz

This means no longer mucking around with

    data/nc-wake_2014, data/nc-wake_2014_q1..., data/nc-wake_2015-02-20, etc.


## 2015-02-03 from #haskell

    13:52 < dino-> quchen: hm, couchbase for Haskell. I could use
      that, is it on hub.darcs.net or github?
    13:54 < quchen> dino-: We're still working on it, it's not
      finished at all yet :-\
    13:55 < dino-> quchen: Fantastic that it's being worked on at all!
    13:57 < quchen> dino-: Glad to hear that! Unfortunately I
      can't open source it right now ("reasons"), but it's going
      to be published under an open license (BSD or something) when
      we're done.


## Getting and processing a year's worth of inspection data

So many I did them in 4 blocks for quarters of the year:

nc-wake data

nc-wake_q1  2014-01-24 2014-04-23
nc-wake_q2  2014-04-24 2014-07-23
nc-wake_q3  2014-07-24 2014-10-23
nc-wake_q4  2014-10-24 2015-01-23


## 2015-01-15 New Couchbase document structure

- Is inspection.violations.id an integer or string across different
  counties?
   - Found violation ids that are alphanumeric in NY state. Example: 2C, 4A
- Any other fields in a violation?
   - Yes, the points they cost individually against the whole score for NC
   - No, there are no points or even scores for NY
- Is reinspection a boolean or more values?


## Isolating subsets of inspection files

From within some directory full of inspection JSON files..

    $ cd data/wake_2014..

The first 100

    $ find . type f | head -100 | xargs cp -v -t ../foo/

The second 100

    $ find . type f | head -200 | tail -100 | xargs cp -v -t ../foo/

The third 100

    $ find . type f | head -300 | tail -100 | xargs cp -v -t ../foo/


## 2014-12-29

correlating inspection data with location data

* go through all JSON from health inspections (in data/)
* for each "location", hit GEO CODE API
* get back doc with lat/lng in "geometry" "location"
* pass lat/lng to Places API (REST)
* interested in locations that match on the stree address number
* in those, interested in geometry location and place_id

Address (from our database) 
    "location": "4121 NEW BERN AVE STE 109 RALEIGH, NC 27610"

Geocoder API 
- they call this a 'forward lookup'
- given a postal address return the geolocation

    https://maps.googleapis.com/maps/api/geocode/json?address=3D4121%20NEW%20BERN%20AVE%20STE%20109%20RALEIGH,%20NC%2027610

    "geometry" : {
            "location" : {
               "lat" : 35.8016467,
               "lng" : -78.5653275
            },


Places API: 
- use the lat & lng returned by Geolocator to find the restaurant

    https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=APIKEYGOESHERE&location=3D35.8016467,-78.5653275&radius=3D50&types=3Drestaurant


* match names 
* match location (from our data) to `vicinity` (Places data)
* note the `score` for the string compare using your algorithm
* note the place_id and geometry from the Places API results


## first notes

Starting form is here: http://wake.digitalhealthdepartment.com/reports.cfm

2015-02: Starting form moved here: http://wake-nc.healthinspections.us/
