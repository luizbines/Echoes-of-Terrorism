This project contains all data, scripts and plots of the "Unsuccessful Terror Attacks Can Still Shape Voting Preferences" paper.

Folders:

- raw: contains all raw data
  - Elections: amount of votes for each party per locality, for all Israel Legislative elections from 2006 to 2022.
  - Israel: Israeli demographic data, localities' coordinates and shapefiles (including the Gaza Strip).
  - Red Alerts: code and output (dataset) of the Israeli siren alarm system Red Alerts.
 
- cleaning: prepares all datasets for analysis
  - Elections: creates datasets for Likud and Right-Wing block percentages per election per locality
  - Israel: creates unique dataset for Israeli demographics per year per locality
  - Red Alerts: cleans Red Alerts dataset and filters only rocket attacks.
 
- treating: runs all regressions and produces all plots
