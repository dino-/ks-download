# ks-download


## Synopsis

Data downloader and parser for the KitchenSnitch project (Haskell)


## Description


## Installing

Installation is usually from a .deb file located on
[github](https://github.com/dino-/ks-download/releases)

Instructions for building the .deb file are below, under Building for
deployment.


## Configuration and execution

Make a directory for config files, and copy all the files from
`/usr/share/ks-download/resources` into it, along with your Google API key in a
file.

The key should be the only thing in the `GoogleAPIKey` file, on a
line by itself. 

You will need to put real credentials in `monbodb.conf` as well.

The directory should look something like this:

      $HOME/
         .config/
            ksnitch/
               GoogleAPIKey
               ks-locate.conf
               mongodb.conf
               .. more conf files from share ..


### ks-locate - Google Places lookup utility

To run, have a directory structure like this:

    inspectionsToProcess/
      fail/
      insp/    <-- this is where you have downloaded insp_ json files
      succ/

Then, to run:

    $ cd inspectionsToProcess
    $ ks-locate -c $HOME/.config/ksnitch -s succ/ -f fail/ --delete insp | tee ks-locate.log

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

    45 1 * * *  /opt/ks-download/bin/ks-regionupd --log-priority=NOTICE $HOME/.config/ksnitch > /some/dir/ks-regionupd.log


## Building from source


### Building for development

    $ stack build
    $ stack exec BIN_NAME -- ARGS
    $ stack test
    $ stack clean


### Building for deployment

Our production servers have so far been Ubuntu, so what follows are
instructions for building a .deb file which can be installed with dpkg.

You will need the [hsinstall](https://github.com/dino-/hsinstall/releases)
utility version 2.5 for this procedure.

Now, starting in the root of the project, do this, where VER is the version of
ks-download you are building:

    $ hsinstall --prefix=ks-download/ks-download_VER/usr
    $ mkdir ks-download/ks-download_VER/DEBIAN
    $ cp -t ks-download/ks-download_VER/DEBIAN util/resources/DEBIAN/control util/resources/DEBIAN/conffiles

Edit `ks-download/ks-download_VER/DEBIAN/control` to make sure the version
matches what you're building.

    $ sudo chown -R root:root ks-download/ks-download_VER
    $ dpkg-deb --build ks-download/ks-download_VER

And you should see a `ks-download/ks-download_VER.deb` file. Check the contents
if you wish with `dpkg-deb -c ...` This file can be added to the ks-download
release page on github or distributed however you wish.

Some of these notes exist in a more detailed form on the developer wiki.

When you want to insert records into the database, make sure you do not forget
to edit the `PREFIX/bin/ks-dl-nightly-XXX.sh` scripts. By default ks-dbinsert
execution is commented out and a fake one runs!


## Contact

### Reporting Bugs

### Authors

Dino Morelli <dino@ui3.info>


## Links
