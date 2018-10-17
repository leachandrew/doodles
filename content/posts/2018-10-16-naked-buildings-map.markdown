---
title: Naked Buildings Map
author: Jens von Bergmann
date: '2018-10-16'
slug: naked-buildings-map
categories:
  - Vancouver
tags: []
description: 'Just buildings.'
images: ["https://doodles.mountainmath.ca/static/images/naked-buildings-map.png"]
featured: ''
featuredalt: ""
featuredpath: ""
linktitle: ''
type: "post"
---







The other day the New Your Times did a [really fun story on buildings in the US](https://www.nytimes.com/interactive/2018/10/12/us/map-of-every-building-in-the-united-states.html). There are several datasets for building data, and we have worked quite a bit with the City of Vancouver LIDAR generated data of 2009. We have mapped this on several occasions in the past, e.g. [to show building heights](https://mountainmath.ca/vancouver_lidar/map), or just [residential buildings to map building values](https://mountainmath.ca/assessment/split_map?zoom=15&lat=49.2509&lng=-123.1243&type=3&use3D=0&year=2017&units=[1,1]), and have used the building heights data in Vancouver and Toronto to make [building heights profiles](https://doodles.mountainmath.ca/blog/2018/05/11/building-height-profiles/) by distance from downtown.

[Robert White did a really nice building map too](http://maps.nicholsonroad.com/heights/).

Another great source, in many ways better for this project, is OSM building data. It's not very good at 3D data, but the footprints are pretty up-to-date.

I really liked the simplicity of the NYT map, so why not make yet another building map? These kind of "naked maps" can be quite revealing, and it reminds of the [naked bike maps from the WaPo WonkBlog](https://www.washingtonpost.com/news/wonk/wp/2015/04/01/bleak-maps-of-how-cities-look-using-only-their-bike-lanes/?noredirect=on) that we [also imitated](https://doodles.mountainmath.ca/blog/2015/04/01/bike-paths/), although it seems that my live maps did not survive my blog update. The [WaPo even made a quiz to have people guess cities by the naked bike maps](https://www.washingtonpost.com/news/wonk/wp/2015/04/03/quiz-can-you-identify-these-cities-by-only-looking-at-their-bike-lanes/?utm_term=.37f20c402ce1).

## Vancouver building footprints



<iframe src="/widgets/m1.html" style="width: 100%; height: 700px; border:0;" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

If you want some context, here is another one with streets and street names.



<iframe src="/widgets/m2.html" style="width: 100%; height: 700px; border:0;" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

## OSM buildings
We do have another source of buildings: Open Street Map. In fact, the above map with context has the OSM building data if you zoom in enough. Here is the map with just the buildings.




<iframe src="/widgets/m3.html" style="width: 100%; height: 700px; border:0;" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>


The problem here is that the buildings disappear when zooming out, that's because the data is served as vector tiles, and buildings are omitted for higher zoom levels.

One way around that is to download the buildings from osm first. We quite like [Geoff Boeing](https://twitter.com/gboeing)'s [OSMnx package](https://github.com/gboeing/osmnx), so that gives us a chance to try out the better python integration in R notebooks in RStudio 1.2. Things are still not as smooth as one would like, and I could not figure out how to pass binary geographic data from python to R, but transforming to geojson gets the job done.












<iframe src="/widgets/m4.html" style="width: 100%; height: 700px; border:0;" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>

The advantage of grabbing the data form OSM is that this immediately generalizes to everywhere where we have OSM building data.

As always, the code for this post is [on GitHub](https://github.com/mountainMath/doodles/blob/master/content/posts/2018-10-16-naked-buildings-map.Rmarkdown). If you want maps for other cities in Canada or elsewhere in the world, just grab the code and modify it to your liking.
