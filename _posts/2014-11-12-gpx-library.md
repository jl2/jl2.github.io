I recently wrote a small library, called [gpxtools](https://github.com/jl2/gpxtools), for processing [GPX](https://en.wikipedia.org/wiki/GPS_Exchange_Format) track files in Common Lisp.

It doesn't have many features yet, but right now it can read a GPX file and compute elevation gain, elevation loss, and the total distance of all the tracks in the file.

Here is a sample usage from the REPL:

``` common-lisp
* (ql:quickload 'gpxtools)
   To load "gpxtools":
   Load 1 ASDF system:
   gpxtools
   ; Loading "gpxtools"
   .....
   (GPXTOOLS)
* (defparameter *gpx*
    (gpxtools:read-gpx "/Users/jeremiah/gpx_tracks/precarious_climb.gpx"))
Processing track: ACTIVE LOG
*GPX*
* (gpxtools:summarize *gpx*)
Total elevation gain: 3567.271946242661d0 feet
Total elevation loss: -3538.885209324304d0 feet
Total elevation loss: 6.437161570619994d0 miles
NIL
*
```

So far the most challenging part of the project was the calculation of distances given points in latitude and longitude.  Doing so requires converting the coordinates to the [Universal Transverse Mercator](https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system) coordinate system which uses meters, and then calculating the distance.

I wasn't able to find a library to do the conversion for me, so I [wrote my own](https://github.com/jl2/utm), and requested it be added to QuickLisp.  It looks like everything went well, and it should be in the next release.

I'm planning to add more features to the gpxtools library, and to clean up the code in a few spots.

One thing I want to do is add generic traversal code and use it to simplify functions like distance, elevation-gain, and elevation-loss that iterate over the entire GPX data structure.
Another thing I want to add is the ability to split tracks at a certain location and selectively write the results back to one or more files.  I'll often start my GPS at the beginning of a hike, but forget to turn off the GPS when I finish hiking and drive away. The result is a GPX track with 10 miles of hiking and 50 miles of driving.  I may even try to do the cut automatically, ending the track when it gets back to (or reasonably close to) the origin, and splitting the drive out into a second track automatically.
