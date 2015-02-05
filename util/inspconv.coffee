fs = require 'fs'
#{ puts } = require 'util'


inDir = "insp"
outDir = "insp-fix"

fs.mkdirSync outDir, 0o0755 unless fs.existsSync outDir

fileNames = fs.readdirSync inDir


transform = (fileName) ->
   insp = JSON.parse fs.readFileSync "#{inDir}/#{fileName}"

   _id = insp._id    # extract the _id field
   delete insp._id   # and then remove it

   # Write the new file
   fs.writeFileSync "#{outDir}/#{fileName}.json",
      # Put _id and remaining data back together new IdInspection shape
      JSON.stringify { "_id": "#{_id}", "inspection": insp }


transform fileName for fileName in fileNames
