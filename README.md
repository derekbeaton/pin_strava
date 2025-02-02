Some Strava stuff
================

# Setup

I’ve forked and adapted a lot of code from [Julian During’s
repo](https://github.com/duju211/pin_strava). For an overview of the
setup and the background (e.g., Strava access token, API) see that repo.

# A better(?) year in review

With access to Strava’s API and a little bit of code, we can make some
real nifty year in review things that are—perhaps—better than what we
typically get from Strava. Plus we can do a multi-year review and even
get back some of types of features that Strava took away from the free
version of the app/site.

# Background

I’ve been running for about 6 or 7 years total, but I’ve only used
Strava to record my (running and other) activities since January 2,
2018. At the time of this (crude) write up: I’m clocking in at around 4
years of Strava data. Which obviously means it’s time to analyze and
visualize those data.

<!-- update this and rewrite it -->

This write-up will walk us through a bunch of visualizations that tells
the story of my running habits and performance over the past 4 years and
also tells me a lot more about what I’ve been doing (and not doing)
especially during a pandemic.

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
Year
</th>
<th style="text-align:right;">
Number of runs
</th>
<th style="text-align:right;">
Total kilometers
</th>
<th style="text-align:right;">
Total minutes
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
<td style="text-align:right;">
1804.548
</td>
<td style="text-align:right;">
8704.167
</td>
</tr>
<tr>
<td style="text-align:right;">
2019
</td>
<td style="text-align:right;">
375
</td>
<td style="text-align:right;">
2195.916
</td>
<td style="text-align:right;">
10849.867
</td>
</tr>
<tr>
<td style="text-align:right;">
2020
</td>
<td style="text-align:right;">
278
</td>
<td style="text-align:right;">
1767.428
</td>
<td style="text-align:right;">
8890.300
</td>
</tr>
<tr>
<td style="text-align:right;">
2021
</td>
<td style="text-align:right;">
198
</td>
<td style="text-align:right;">
1797.200
</td>
<td style="text-align:right;">
9115.367
</td>
</tr>
</tbody>
</table>

In 2020 and 2021, I went a lot more walks (with beer and through
Toronto’s lovely ravine system) and I lucked out and was able to buy a
bicycle, so a lot of other activities aren’t here (that’ll be a separate
write up).

Before I explain what’s going on in 2019 and 2021 (though you probably
have a guess or two for what happens from 2020 onward), let’s take a
look at how these activities break down. I’ve been running long enough
to know what kinds of running I like and what kinds of running I really
don’t like. And I’m pretty in tune with the general types of runs I do.
So, one of the first things I did with my data was to label each run
based on a “distance class”:

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

![](./figures/proportion_runs_ggplot_facet.png)<!-- -->

Generally, I keep my runs in the 5-10km range, which we see year over
year. However, there are some big differences between the years:

-   In 2018 I started doing actual training instead of just running
    whenever, which explains the proportion of runs in the 1-5km range
    (these are a lot of cool downs and warm ups).

-   In 2019 I ramped up my training (considerably) and ran more official
    races than before: two 5ks, one 10k, one 8k (Spring Run Off), one
    overnight trail relay that included 3 separate legs per person (easy
    7.5k, medium 8k, and a hard 9k), one 30k (Around the Bay), and one
    half marathon. So there’s a lot more diversity in my runs because of
    training and races. So in 2019 we see a big spike in the &lt;= 1km
    and 1-5km runs (workouts, warm ups/cool downs), as well as a jump in
    the 24+ km runs.

-   In 2020 I was on my new found training and racing schedule until
    about March 15th 2020, which was a **big day**: my first virtual
    race (Achilles 5k) and my last meal indoors since (RIP [Dave’s on
    St. Clair](https://www.youtube.com/watch?v=8nvzEqsZIGo)). Then I
    effectively stopped any form of training and kept things mostly
    casual.

-   In 2021 I had grown a bit lazy and burnt out (not of running, just
    sort of of everything). But I wanted to keep some level of fitness
    and tried to commit to 40km per week. However for almost the first
    4-5 months of 2021, I did that by running the same 10km route 4-5
    times a week (you’ll see this in the route visualization). So that’s
    why there’s considerably more runs (proportionally) in the 5-10 and
    10-15km range.

# When do I run?

![](./figures/clock_ggplot_facet.png)<!-- -->

I’m an after work runner (I’m also not particularly a morning person).
Each year we can see that tons of my runs start in the 5pm, 6pm, or 7pm
hours. However, there are some noticeable spikes in 2018 and 2019 for
mornings and around noon. These earlier in the day runs are one of three
types of runs: (1) runs at lunch (“runch”) on weekdays, (2) getting
dragged out far too early for my liking to run with others
(Saturdays/Sundays), or (3) a perfectly reasonable running time at
around 10am or 11am with [the only running group that
matters](https://www.runtobeer.ca/).

Almost all of my runs in 2020 and 2021 have been on my own, so there are
few earlier runs. There’s also an interesting shift over the years:

-   2018: I’d usually run after work once I got home, usually sometime
    just around or after 5:30pm

-   2019: A lot of Wednesday and Friday runs include workouts with
    running groups, and these were usually 6-7pm start times

-   2020 and 2021 show the most interesting shift in running start times
    from about 5pm to about 6pm. This shift happened because I got a new
    job: what I do changed, and with that change, I also changed my
    working hours a bit. I also haven’t gotten up before 8:00am since
    about March 15, 2020 because (1) I just roll out of bed and am at
    work and also (2) mornings are bullshit

# Distance, time, and pace

I’m a very stupid runner. This is something I’ve known about myself for
a while. For examples: I don’t take hydration or nutrition on long runs
(which explains why I come home and just lay down on the floor). I also
have no idea what kinds of paces I run or should be running. I just sort
of go. For about 3 years now, I’ve had a sense of this, but I wasn’t too
sure because I don’t really track what I do (yet more evidence I am a
stupid and/or lazy runner).

## Overview

Is my sense—that I just sort of run—vaguely correct? Each activity from
Strava has time and distance, so we can use these data to find out.

![](./figures/distace_time_viz_ggplot.png)<!-- -->

With the exception of a few outliers (which we find out about later):
That’s a solid yes. The above plot shows all of my runs (each individual
dot) over 4 years. We see distance on the horizontal axis and time on
the vertical axis. The data form a pretty clear line, which means I
basically run about the same pace, no matter how far I’m running. This
is somewhat convenient for me (because I’m lazy) so I can basically
compute how long any given run will take me because it’s a simple
multiplier. A 10k takes me twice as long as a 5k and a 20k takes me
twice as long as a 10k and four times as long as a 5k. This same
multiplier also works for how many slices of pizza or donuts I eat after
a run (please don’t question this as I only pretend to be good at math).

Let’s break some of these data down by year.

![](./figures/distace_time_viz_ggplot_facet.png)<!-- -->

With each year, we have a bit more of a sense of what I do. In 2019,
there was much more variation in paces, especially as runs became
longer: I’d run fast (usually a race) or I’d run slow (usually with
others or a group). But in 2020 (and 2021), I ran alone and with no
(real) races, so I ran in a very linear way: at almost the same pace
regardless of distance.

<!-- And now for fun, let's identify which of these activities are races. -->
<!-- ```{r, echo=FALSE} -->
<!-- knitr::include_graphics(tar_read(distace_time_viz_race_png_facet)) -->
<!-- ``` -->

## Daily break down

Time for the nitty gritty: a daily breakdown over 4 years of how far I
ran (distance in kilometers), how long I ran (time in minutes), and pace
(minutes per kilometer). The following visuals are all the same type of
format: calendar heatmaps that show each day as a little box, with
months across the top, and all of the months grouped together as years
based on the rows. The little numbers inside each box are the rounded
value (total distance, total time, average pace), where the color
reflects a high or low value (relative to all other values).

This daily breakdown really helps me understand the above plots in a way
where I can tell you exactly what each of those outliers were. I can
even tell you a lot more about each of these days just by looking at the
following visuals. In almost every case: I can tell you what was
happening that day with no help from external sources (I had to go back
and look up one and verify one other).

So I’ll now tell the story of some of these stand out items. After the
individual stories, there are a few global stories to tell. First up:
amount of running time per day.

![](./figures/calendar_heatmap_time_ggplot.png)<!-- -->

Though I know a few of these, there’s one particular stand out event:

-   A Saturday in late August, 2018: One of original Rotman/Baycrest
    Brain Runs, where a bunch of us who are runners and were at Rotman
    recruited others and went on a running tour of the [Baycrest
    Brains](https://www.brainproject.ca/) throughout Toronto. This was a
    multi-leg run with a lot of people that we picked up along the way.
    And we also spent a lot of time at each of the brains. I also never
    paused my Strava which explains the highest amount of time. You also
    see this stand out in the distance graph (very purple, so a lot of
    distance) and the pace graph (very green which is slow, more than 9
    minutes per km).

I have far more vivid memories of distance and pace. So let’s move on to
those. First up: distance.

![](./figures/calendar_heatmap_distance_ggplot.png)<!-- -->

There are some relatively big numbers here (very purply) per year and I
can tell you what they were:

-   A Wednesday in late December, 2018: My first long training run in
    preparation for my first (and so far only) Around the Bay, which is
    a 30km race. This is perhaps my favorite long run of all time
    because it was on the Isle of Palms (South Carolina). When I
    finished my run, I ended at the Harris Teeter so I could pick up
    some (well earned) beer to enjoy with family that night. The most
    important detail of this run is it resulted in one of my many
    celebrity encounters, and it was the only celebrity two-fer:
    [Paul F. Tompkins and Janie Haddad
    Tompkins](https://twitter.com/stayfhomekins) were also shopping at
    this Harris Teeter and I just sort of stared at them from like 10
    feet away. I was very sweaty and smelly so it was for the best that
    I kept my distance. But this is also how most of my celebrity
    encounters happen: I just sort of stare at celebrities from a
    distance and pretend it’s “an encounter”.

-   A Saturday in late February, 2019: This was a double run day: One of
    my longest training runs (\~32 km) in some very awful weather,
    followed up by a 5k RunTOBeer pre-Achilles 5k run. Between the two
    runs I went to a coffee shop for an iced Americano and a scone
    (literally the worst hydration and nutrition choices), and then
    ended up at Steam Whistle and drank pilsner out of 32 ounce milk
    jugs (literally the best hydration and nutrition choice).

-   A Sunday in late March, 2019: This was Around the Bay. The only time
    I’ve done it, and this was longest race I’ve done. I learned two
    things that day: (1) Merit’s Table Saison was the perfect post-race
    beverage, as it clocked in around 3% and I could just enjoy these
    until it was time to get a bus back to Toronto, and (2) racing
    results in strange physiological responses: once the race was over,
    I was wandering the exhibit hall for a bit and found a place to just
    sit down and rest. After about 15 seconds of sitting down I burst
    into tears and cried uncontrollably and harder than I’ve ever cried
    before. After about 30 seconds, I stopped crying as abruptly as I
    started and then felt a sense of euphoria. Running is such a dumb
    sport.

-   A Sunday in early March 2020: I went on a training run (20 km),
    paused for an iced Americano and a scone (you can see a pattern
    developing) and then joined in on the last pre-pandemic RunTOBeer
    run (10 km), which was our big hurrah before the Spring Run Off race
    (or so we *thought*: not really, we all knew what was coming)

Now that it’s clear that my running is fueled by beer, coffee, and
scones but that it also results in inexplicable crying, let’s move to
running *pace* per day (minutes per kilometer).

![](./figures/calendar_heatmap_pace_ggplot.png)<!-- -->

With the visualization of the paces, there are a few individual stories
that pop out.

-   A Friday in early June 2019: A group run with neuroscientists in
    Rome. We ran to and along various sites, and ended up having pizza
    and beer (do as the Romans do, or so they say)

-   A Sunday in late March 2019: The Achilles 5k. The fastest and
    hardest I’ve ever run, and the best performance I’ve ever had. I was
    not in any way preparing for this, because I was not training for a
    5k. I was in the training cycle for a 30k. But this 5k taught me how
    important long training runs are for short racing runs: you’re
    building a lot of strength and endurance over the long runs that
    really pays off for short runs. Between January 1, 2019 (a 5k in
    Charleston, SC) and mid March 2019 I shaved about 1:20 off of my 5k,
    from about 20:45 to 19:22. Though in some fairness, that run in
    Charleston had some precarious conditions: It was about 27 degrees
    (celsius), very humid, and **very hungover**.

-   A Sunday in late January 2020: The Frozen Pilgrim 5k. This was
    another sub 20 minutes 5k during a training cycle for Around the
    Bay. Like a previous 5k, the conditions were also precarious (it was
    fairly cold and I was **very hungover**)

-   A Sunday in late November 2021: The Tannenbaum 10k. Pretty much the
    only pink/purple box of 2021. This was my first in person race in 2
    years: the last one I had done before that was the Tannenbaum 10k in
    2019 but *technically* that one was cancelled because of a [Bomb
    Cyclone](https://en.wikipedia.org/wiki/Explosive_cyclogenesis). But
    given that all the racers were on site, the organizers basically
    threw their hands up and said “we’ve cancelled the race but you can
    run if you want” ([and so we
    did](https://twitter.com/derek__beaton/status/1336042055848620034/photo/2)).
    For 2021 it was a *near repeat* of 2019: A storm rolled in and it
    wasn’t great to race in. But it was good to run an actual race again
    (especially because it was in Tommy Thompson park)

## Some global stories

From the above 3 calendar/heatmap plots, there are a few global stories
that emerge.

-   Noticeable gaps in the summer months in 2020 and 2021: I bought a
    bicycle in July 2020 and really leaned into that, but purely in a
    literally fairweather way. Saturdays and Sundays became long ride
    days and this is **particularly noticeable** on Saturdays from the
    end of April 2021 to the end of October 2021 where there are a total
    of 2 Saturday runs over 6 months.

-   I’ve slowed down considerably in 2020 and 2021. A real head
    scratcher on why.

-   I apparently run my best 5ks when I’m **very hungover**

# Next up

There are tons of data that can be pulled from the Strava API. The next
part(s) of my Strava story will be on my running routes and then my
significant changes during the pandemic (lots more biking and walking)
