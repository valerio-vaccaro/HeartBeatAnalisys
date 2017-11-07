# HeartBeatAnalysis

## Introduction
Based on the results of [My Activity Dashboard](https://valerio-vaccaro.shinyapps.io/MyActivityDashboard/) (developed for the Coursera course called Developing Data Products) this application allow the analysis of sportive activities inside the R platform.

It allow you to see:

- the map of the trip using Leaflet
- information about heart rate on temporal base and on historgram
- information about speed on temporal base and on historgram

The dasboard is able to filter speed and heart rate date in order to select a subset of observations.
 
## Hardware supported 
The app is tested with data recored using a Motorola Moto G Android phone with the [SenseView application](https://play.google.com/store/apps/details?id=si.mobili.senseview) installed and connected to a [Polar H7 belt](http://www.polar.com/us-en/products/accessories/H7_heart_rate_sensor) in order to record the heart rate data. 

Information from the internal GPS and acceleration sensor are also saved and exported at the end of the trip.

The complete code for read the data recorded from the SenseView application was developed for this project.
