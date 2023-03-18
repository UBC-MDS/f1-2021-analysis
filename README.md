# Visualising the Formula 1 2021 Season

<!-- badges: start -->
[![App Deploys](https://github.com/UBC-MDS/f1-2021-analysis/actions/workflows/deploy-app.yaml/badge.svg)](https://github.com/UBC-MDS/f1-2021-analysis/actions/workflows/deploy-app.yaml)
[![Test app w/ {renv}](https://github.com/UBC-MDS/f1-2021-analysis/actions/workflows/testing.yml/badge.svg)](https://github.com/UBC-MDS/f1-2021-analysis/actions/workflows/testing.yml)
<!-- badges: end -->

## Welcome!

Whether you consider yourself a true Formula 1 veteran, or this is the first time you've heard of the sport, we bid you welcome to: [our dashboard!](https://eric1208.shinyapps.io/f1-2021-analysis/)  

Firstly, a quick overview of what you can find on this page:

- [Visualising the Formula 1 2021 Season](#visualising-the-formula-1-2021-season)
  - [Welcome!](#welcome)
  - [The Contributors](#the-contributors)
  - [The Motivation](#the-motivation)
  - [The Need-to-Know](#the-need-to-know)
  - [The Description](#the-description)
  - [The Contributing Guidelines](#the-contributing-guidelines)
  - [The License](#the-license)
  - [The Credits](#the-credits)

![Lights out](/www/sketch/lights_out.png)

## The Contributors

We are four students at UBC, Vancouver, part of the Master of Data Science program:

- [Tanmay Agarwal](https://github.com/tanmayag97)
- [Julie Song](https://github.com/YXIN15)
- [Eric Tsai](https://github.com/erictsai1208)
- [Renzo Wijngaarden](https://github.com/RenzoWijn)

## The Motivation

The 2021 Formula 1 season was one of the most exciting in the history of the sport. A title fight in the drivers championship that left both the neutral viewer and fans of either Red Bull's Verstappen and Mercedes' Hamilton on the edge of their seat and beyond until the very last lap of the year in Abu Dhabi, and an equally as engaging brawl in the constructors midfield makes no one deny its legendary status.  

With all these developments throughout the season's calendar it can be hard to remember how it all went down. We are looking to provide a helping hand in reliving this awesome year, by summarising and visualising all the important statistics of 2021!

## The Need-to-Know

Of course not everyone is as familiar with Formula 1 as the most dedicated fan, so here is a super quick run down to get you up to speed:

The Formula 1 season runs from the start to the end of the calender year, with the first race usually taking place around March, and the last one around December. In one season, 20 drivers spread out over 10 teams try and accumulate as many points as possible. Any point they score counts towards two different competitions: the Drivers Championship  and the Constructors Championship. This means that the driver that has the most points after the last race is World Champion, and the two drivers that race for the same team who have the most total points combined, make their team the Constructors Champion.

So how does one get these points? Each weekend, the drivers get three practice sessions, after which they continue to qualifying. During qualifying they have to try and drive as fast as possible for one single lap. Then, the order of who drove the fastest lap determines the starting grid for the race on Sunday.  

On race day, the drivers race for 300 kilometers around that weekends track, in a spectacle that isn't just about who can drive the fastest, but also about what cars have the best design, what team can deliver the fastest pitstops, and what driver manages to stay out of collisions.

Then, once the 300 kilometers are up, the order in which the drivers cross the finish line determines their position for that race, and more importantly how many points they get! And additionally, the driver that set the fastest lap time during the race, receives one extra bonus championship point.



## The Description

![Landing Page](/www/sketch/f1-dash-demo1.gif)

Our app opens on the Season Highlights page. On the left hand side you can either select Drivers or Teams: this selections determines what is shown in the cumulative season points plot to its right. Underneath the plot is a slider with which you can adjust the number of races shown in the plot. On the right hand side is a table with all the races that happened in 2021, in order of which they took place. Adjusting the slider will highlight the races in the table that are currently included in the selection.

![Race Page](/www/sketch/f1-dash-demo2.gif)  

The second page is the Race Information tab. In the top left there is a dropdown selection box where you can select what race you'd like to have displayed, together with previous/next buttons to navigate race-by-race. Underneath this is a picture of the track layout, along with a table containing facts about the corresponding Grand Prix. To its right is the results table: this shows the order the race finished in, but can also be sorted by starting position, fastest lap, and any other column displayed. In the top right is a search bar in order to filter the table by a search query. The fastest lap driven is highlighted purple in this table.

## The Contributing Guidelines
Do you have ideas on how we can improve our dashboard, and are you interested in contributing? We'd love to see your suggestions! To make changes locally just clone the repo, navigate to the top folder, and run the app:

```
git clone git@github.com:UBC-MDS/f1-2021-analysis.git # If SSH is set up

cd f1-2021-analysis

RScript app.R
```

Check out the [contributing guidelines](CONTRIBUTING.md) if you're looking to make additions to our project! Please note that this project is released with a [Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
## The License
`f1-2021-analysis` was created by Tanmay Agarwal, Yingxin Song, Renzo Wijngaarden, and Eric Tsai. It is licensed under the terms of the MIT license.
## The Credits
The datasets used in this app are from [Formula 1 World Championship (1950 - 2022)](https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020?select=lap_times.csv) on Kaggle and from [Formula 1 Datasets](https://github.com/toUpperCase78/formula1-datasets) on Github. The flag images are from [the country flags repository on GitHub](https://github.com/hampusborgos/country-flags).
