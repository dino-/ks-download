- Do we need ks-docdump and ks-dump any longer? Probably one of them can go.
- Do something with that scrubName business
- Can we factor out the formatTime stuff we're doing at least twice in this code? Make it part of the ks-library
- Change mongo password (actually everything), it's insecure now
- [done] Figure out where the missing data is, import everything again to be clear on it
- Organize the new Mongo stuff
- Add function:
   - KS.Data.BSON.bsonToDoc :: BSON.Document -> D.Document
- Split the mongo task up:
   - First, convert all of the documents from couch format to mongo, on disk
   - Then load all of those and upsert into mongo

- Triangle potential population of Android users
- Add use case and caching info to the new wiki
   - use cases:
      - what's around a specific location
      - search for a specific restaurant
   - caching on the client:
      - ??
- Add msmtp installation notes to wiki

- Fill out more of README.md
- It may be time to put info in changelog.md
- Look into migrating to MongoDB
- Is there a different url encoding API than what's in Network.HTTP?
- KS.Data.Inspection
   - Should we really be using Data.ByteString.Lazy.Char8 for
     saveInspection? Try with just Data.ByteString.Lazy or even
     not lazy.
- ks-dlinsp
   - Failure to access the URL kills the whole process. Should we handle this with retrying somehow? Not sure
   - Should have much more/better logging
- Ks.DlInsp.Source.*
   - Replace usage of Network.HTTP with Network.HTTP.Conduit
     (http-conduit). See KS.Locate.Places.Geocoding
