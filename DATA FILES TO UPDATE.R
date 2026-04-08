###What data files to update

##Final CPP Data - remove column for oldest data- should have 12 data points for each indicator
##Final IGZ Data - remove col for oldest data - should have 15 data points
##Final DZ Data

##datazone data for maps.xlsx - need to update with most recent dz data

##Metadata.csv - update years and any methodology changes

#Optional: If there are new SIMD income dep rankings - update DZ & IGZ income data.xlsx

##AND Duncan Index stuff
### - create new IZ data file in DIDta folder
### - can keep columns E-G the same, unless a new SIMD has been published
### - replace all data with the most recent raw data for each column ("PD" is participation)
### - historic years' files may need changed too esp. for child poverty and depopulation 
### - update code called "Final code for..." with your new file (row 52 and 54) and run it
### - copy the output into one of the other excel files called e.g. Duncan index by FINALSep25 
### - delete all rows that are not raw data in the "Flipped data to use" tab


####All other files created by code!


##Should also add something to the global file with the most recent and next update - line 208

##global.r - update dates in lines 26-28 and 34-36
##server.r  - change files in lines 1714 and 1723
##ui.r - line 505 on update date, 509 link to methodology, 540 for version history

##send IZ and CPP data to zipped folder for download