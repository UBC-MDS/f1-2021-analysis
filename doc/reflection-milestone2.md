# Reflection

We have implemented most of the features we initially specified in our proposal and sketches except for two features which will be described below. Our app has two tabs that show different information depending on the tab selected.

In the Season Highlights tab, we implemented a reactive plot that shows the driver’s cumulative points gained throughout the season over the races. The user can filter for specific drivers using the checkboxes and examine the points gained in detail by sliding the slider to view the points gained chronologically over the races in the season. On the right side of the page is a table of all the races which includes the race name (usually just the country’s name) and the city it was held in. As the values in the slidebar change, the races in the table will be highlighted to show the currently selected races. We also implemented the same functionality to view the similar information for teams by switching the tab from Driver to Teams.

In the Race Information tab, we implemented separate tables that show information about the race circuit and the race result depending on the race selected in a dropdown menu. Above the race circuit information table is an image of the circuit. In the race result table, the shortest time for the “Fastest Lap” column is highlighted, as this denotes the fact that the driver is awarded one additional point.

Overall, we feel that the Season Highlights tab does a good job at summarizing the overall race season, while the Race Information tab allows the user to dive into information about specific races within the season. We were unable to implement the line chart showing the lap times of drivers for each individual race due to the lack of time this week with all the other labs. We decided not to implement the world map that allows the user to select the races around the world because we feel that it is unnecessary and clutters the app. The same information is easily captured in the Race Information tab.

For the remaining milestones, we aim to finish implementing the plot for the lap times as well as polish up the app to make it appear more aesthetically pleasing. We will implement the plot for lap times in a separate tab as opposed to in the same Race Information tab as we believe that will clutter the page. One enhancement we hope to implement is to make the checkboxes for selecting drivers/teams look nicer by adding the driver’s headshot or the team’s logo beside their name. We are also considering making the colour of the lines in the plots match their team’s colour scheme (e.g. McLaren -> orange, Ferrari -> red).

The following details regarding features and bugs should be noted: 

* Occasionally the label “Emilia Romagna” will not show up in the slidebar. This is a known bug but we are currently unsure of the fix. We suspect that it may be due to the lack of space available on the slidebar. 

* The line plot when filtering for driver Robert Kubica shows only a short segment of the Dutch and Italy race. This is expected since he was a reserve driver and raced for only those two races

* The information for the Belgian Grand Prix may appear incorrect at first glance in the Race Information tab. However, the information shown is correct because the race was canceled due to the weather.

* For the GPs in Azerbaijan, British, Belgian, Mexico City, the “Fastest Lap” column has no value highlighted. This is a feature due to the fact that no additional point was awarded if the driver who had the fastest lap did not finish within the top 10 places. However, we are considering making the change to highlight the fastest lap time even if the driver did not finish within the top 10 places.
