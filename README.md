# borealLynx

This is a shiny app for visualizing and summarizing GPS collar data from lynx in Alaska. 
Currently, users upload a .csv file that at a minimum contain unprojected GPS fixes 
(lat/long), collarIDs and dates. These are read in and mapped using leaflet.
A summary table is produced that includes includes refuges and fist and last fix
dates, by collarID. Users can subset the data by date range, collarID, and Refuge.

Next steps include developing a home range map viewer and the ability to generate
a report using markdown that includes information on fix success, movement status
(resident vs. migratory behavior), summary movement statistics, and live/dead status.