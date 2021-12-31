Some Strava stuff
================

# Setup

I’ve forked and adapted a lot of code from [Julian During’s
repo](https://github.com/duju211/pin_strava). For an overview of the
setup and the background (e.g., Strava access token, API) see that repo.

# Background

I’ve been running for about 6 or 7 years total, but I’ve only used
Strava to record my (running and other) activities since January 2,
2018. At the time of this (crude) write up: I’m clocking in at around 4
years of Strava data. Which obviously means it’s time to analyze and
visualize those data.

This writeup will walk us through a bunch of visualizations that tells
the story of my running habits over the past 4 years and also tells me a
lot more about what I’ve been doing (and not doing) especially during a
pandemic.

The visuals are all taken from a minimal set of activity level data,
such as time of day, distance, time, and so on. As of now, there’s no
information on routes and high frequency data such as the longitude and
latitude during runs (that’s for part 2).

# How many runs and what kinds of runs?

I’ve recorded 1111 running activities in Strava (manual entries
excluded). Per year, that comes out to:

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
year
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
2018
</td>
<td style="text-align:right;">
260
</td>
</tr>
<tr>
<td style="text-align:right;">
2019
</td>
<td style="text-align:right;">
375
</td>
</tr>
<tr>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
278
</td>
</tr>
<tr>
<td style="text-align:right;">
2021
</td>
<td style="text-align:right;">
198
</td>
</tr>
</tbody>
</table>

Before I explain what’s going on in 2019 and 2021 (though you probably
have a guess for 2021), let’s take a look at how these activities break
down. I’ve been running long enough to know what kinds of running I like
and what kinds of running I really don’t like. And I’m pretty in tune
with the general types of runs I do. So, one of the first things I did
with my data was to label each run based on a “distance class”:

-   Less than or equal to 1 kilometer (“&lt;= 1km”): These are usually
    workouts or “specialty races” (e.g., relay version of a beer mile).

-   Between 1 and 5 kilometers (“1-5 km”): These are almost always
    either (1) a slow warm up or cool down or (2) a fast workout

-   Between 5 and 10 kilometers (“5-10 km”): My go-to when I just want
    to run, or when I really really don’t want to run but “I have to”

-   Between 10 and 15 kilometers (“10-15 km”): I like to use these to
    pretend I’m doing a long run (these are not long runs).

-   Between 15 and 18 kilometers (“15-18 km”): My typical range for
    “long runs”. This is because I don’t really like running for very
    long distances: I enjoy half marathons and that’s about it. I refuse
    to consider running a marathon.

-   Between 18 and 21 kilometers (“18-21 km”): The more appropriate
    range for long runs for the types of training I go for (half
    marathons and 5ks)

-   Between 21 and 24 kilometers (“21-24 km”): Often someone has tricked
    me into running this distance or I’ve committed to something I
    grumble about.

-   More than 24 kilometers (“24+ km”): I was definitely tricked into
    these and as soon as I get back home I just lay down on the floor
    because my brain just stops working

So how many of each type do I do?

![](./figures/proportion_runs_ggplot.png)<!-- -->

So how many of each type do I do?

![](./figures/proportion_runs_ggplot_facet.png)<!-- -->
