# Assimilate data sources 

Currently the data being assimilated include:

  - Ocean Science Division survey (CTD casts)
  - Snow crab surveys
  - Groundfish survey temperature profiles
  - Volunteer fisher contributions
  - Misc survey data 

Mostly we are focussed upon the bottom benthic environment. 

This step is an example of data processing and QA/QC that might be relevant to your own area of focus. Data assimilation need simply adjusted to point to your own data.

## Set up parameters controlling spatial and temporal domains

```r
year.assessment=2023

p = aegis.temperature::temperature_parameters( yrs=1950:year.assessment )  # these are default years

# ------------------------------
  # 0 data assimilation

  if (historical.data.redo) {

    # these are no longer relevant:
    #   # data dumps from various sources
    #   temperature_db( p=p, DS="USSurvey_NEFSC" ) # US data .. new additions have to be made at the rawdata level manually
    #   temperature_db( p=p, DS="lobster.redo" ) # FSRS data ...  new additions have to be made at the rawdata level manually
    #   temperature_db( p=p, DS="misc.redo" ) # various survey data, mostly scallop ...  new additions have to be made at the rawdata level manually until a few db approach is finalized
    #   temperature_db( DS="osd.rawdata.1910_2008", p=p ) # redo whole data set (historical) from 1910 to 2010
    #   temperature_db( DS="osd.rawdata.2008_2016", p=p ) # 2008:2016, 2008 is overlapping ... overwrite the older series
    #   # temperature_db( DS="ODF_ARCHIVE", p=p, yr=1969:2015 ) # specify range or specific year .. not used .. here in case the data series gets reactivated .. will need to be brought into profiles.annual.redo
    #   # NOTE:: if groundfish data profiles are maintained by groundfish databases again then an easy way wold be
    #   # to update aegis.survey::groundfish_survey_db( DS="gshyd.georef" ) to assimilate the data ..  this would also mean that 00.surveys.r would need to be run first. ..


  # Roger Petipas has been maintaining a database, the following loads this data
  # the data needs to be unzipped and the Data* files need to be moved into:
  #   project.directory("aegis", "temperature", "archive", "profiles")
    temperature_db( DS="osd.rawdata.1910_2008", p=p ) # redo whole data set (historical) from 1910 to 2010
    temperature_db( DS="osd.rawdata.2008_2016", p=p ) # 2008:2016, 2008 is overlapping ... overwrite the older series
    temperature_db( DS="osd.rawdata.annual", p=p, yr=2018:year.assessment ) # specify range or specific year
    temperature_db( DS="osd.profiles.annual.redo", p=p, yr=2008:year.assessment    )  # no longer used
     
  }

```

## Subset bottom data only for analysis.

This step requires an Oracle data base connection. You need simply replace with your own flat files or RDBMS systems that you use. It essentially copies and formats data into annual slices. 

```r

  o = temperature_db( DS="bottom.annual.rawdata.redo", p=p, yr=1970:year.assessment )  # brent and amy's new db view

```




## Now aggregate all data 

Finally, extract and aggregate time slices into a one data file for use in modelling.

```r

  # Extract bottom data from each profile and discretization of space and time resolution to manageable numbers
  # temperature_db( DS="bottom.annual.redo", p=p, yr=1900:year.assessment )
  o = temperature_db( DS="bottom.annual.redo", p=p,   yr=1970:year.assessment  ) # "basedata"

  o = temperature_db( DS="bottom.all.redo", p=p, yr=1970:year.assessment  )  
  o = temperature_db( DS="aggregated_data", p=p, redo=TRUE )   # used to reduce data size and do quick empirical look ups (not modelled)

```
Continue to the next step: [modelling](03_temperature_carstm.md)

# end



