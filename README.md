# kitchensnitch-dl


## Synopsis

Data downloader and parser for the KitchenSnitch project (Haskell)


## Description


## Installing


## Configuration and execution

`ks-locate.conf`, `ks-dbinsert.conf` and `GoogleAPIKey` files will be
looked for by default in `.` relative to where you ran `ks-locate`
or `ks-dbinsert` from. But it can be nicer to run from within
a directory tree where inspection files and output files will
be written.

To set up for running like this:

Make a directory for config files, and copy the two conf files into
it, along with your Google API key in a file. This can be anywhere
as long as they're together. How about in `$HOME`, like this:

      $HOME/
         .config/
            kitchensnitch/
               GoogleAPIKey
               ks-locate.conf
               mongodb.conf

The key should be the only thing in the `GoogleAPIKey` file, on a
line by itself. 


### ks-locate - Google Places lookup utility

To run, have a directory structure like this:

    inspectionsToProcess/
      fail/
      insp/    <-- this is where you have downloaded insp_ json files
      succ/

Then, to run:

    $ cd inspectionsToProcess
    $ ks-locate -c $HOME/.config/kitchensnitch -s succ/ -f fail/ --delete insp | tee ks-locate.log

When it's finished:

    inspectionsToProcess/
      fail/    <-- contains failed insp_ files
      insp/    <-- this directory is now empty
      succ/    <-- contains successful lookup ks_ files

Running in this way will allow `ks-locate` to find its conf files
and use relative paths for all those directories, which is nice.


### ks-dl-nightly.sh - Utility to run every day to do everything

For `nc_wake`, no inspections are added on weekend days. The `cron`
job shoulld look something like this:

    7 1 * * wed,thu,fri,sat,sun  /opt/ks-download/bin/ks-dl-nightly.sh


### ks-regionupd - Update of regional statistics, to be run daily

Put this in a cron job:

    45 1 * * *  /opt/ks-download/bin/ks-regionupd --conf-dir=$HOME/.config/kitchensnitch --log-priority=NOTICE > /some/dir/ks-regionupd.log


## Building from source

Make sure you have `ghc 7.8.x`, `cabal-install` and `darcs` installed.

Update your cabal list

    $ cabal update

And install some native deps that `cabal` can't do for you

On Ubuntu:

    # apt-get install --reinstall g++ 
    # apt-get install libzip-dev

On Arch Linux:

    # pacman -S libzip

** WARNING These notes below need to be in a more common place, like the wiki **

Make a directory for all of the KS components and their dependencies. This helps us to share a sandbox and minimizing build complexity.

    $ mkdir -p kitchensnitch/.cabal-sandbox
    $ cd kitchensnitch

First, let's install the geojson library into a sandbox

    $ mkdir geojson
    $ cd geojson
    $ cabal sandbox init --sandbox=../.cabal-sandbox
    $ cabal install geojson
    $ cd ..

A version of the aeson-bson library from GitHub will need to be installed

    $ git clone https://github.com/dino-/aesonbson.git
    $ cd aesonbson
    $ cabal sandbox init --sandbox=../.cabal-sandbox
    $ cabal install
    $ cd ..

A version of the bson-generic library from GitHub will also need to be installed

    $ git clone https://github.com/dino-/bson-generic.git
    $ cd bson-generic
    $ cabal sandbox init --sandbox=../.cabal-sandbox
    $ cabal install
    $ cd ..

The ks-library needs to be installed next

    $ darcs clone http://hub.darcs.net/dino/ks-library
    $ cd ks-library
    $ cabal sandbox init --sandbox=../.cabal-sandbox
    $ cabal install
    $ cd ..

Get the `ks-download` source code

    $ darcs get http://hub.darcs.net/dino/ks-download

Update your cabal library and tools, we need a modern version

    $ cabal install Cabal cabal-install

Set up a sandbox for building (if you wish to use a sandbox)

    $ mkdir ~/.cabal/sandbox
    $ cabal sandbox init --sandbox=$HOME/.cabal/sandbox/kitchensnitch

Then install the dependencies

    $ cabal install --enable-tests --only-dep

This will build for quite some time, when it's done, you can build
ks-download:


### Building for development

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

And you should be good for development from here.


### Building for deployment

This will build everything into a deployable directory structure
that you can put somewhere like `/opt/` for instance.

    $ cabal install --prefix=/tmp/ks-download-VER --datasubdir=.
    $ pushd /tmp
    $ tar czvf ks-download-VER.tgz ks-download-VER
    $ popd

When you want to insert records into the database, make sure you do not forget to edit the `bin/ks-dl-nightly.sh` script. By default ks-dbinsert execution is commented out and fake one runs!


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
