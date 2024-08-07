
temperature_db = function ( p=NULL, DS, varnames=NULL, yr=NULL, ret="mean", dyear_index=NULL, sppoly=sppoly, redo=FALSE, ... ) {

  # over-ride default dependent variable name if it exists

  if ( is.null(p))  {
    p_add = list(...)
    if (length(p_add) > 0 ) {
      p = temperature_parameters(...)
    } else {
      p = temperature_parameters()
    }
  }

  voi = NULL
  if (exists("stmv_variables", p)) if(exists("Y", p$stmv_variables)) voi=p$stmv_variables$Y  # used in stmv
  if (is.null(voi)) voi="t"  # default


  # manipulate temperature databases from osd, groundfish and snow crab and grid them
  # OSD data source is
  # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/climate_e.htm
  # http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html
  ## must download manually to this directory and run gzip
  ## use choijae/Jc#00390
  ## depths: 500,500, "complete profile"   .. raw data  for the SS
  # (USER Defined -- region: jc.ss")

  # no time records, just day/mon/year .. assume utc

  basedir = project.datadirectory("aegis", "temperature" )
  dir.create( basedir, recursive=T, showWarnings=F )

  loc.archive = file.path( basedir, "archive", "profiles")
  dir.create( loc.archive, recursive=T, showWarnings=F )

  loc.bottomdatabase = file.path( basedir, "archive", "bottomdatabase"  )
  dir.create( loc.bottomdatabase, recursive=T, showWarnings=F )

  loc.basedata = file.path( basedir, "basedata", "rawdata" )
  dir.create( loc.basedata, recursive=T, showWarnings=F )

  loc.profile = file.path( basedir, "basedata", "profiles" )
  dir.create( loc.profile, recursive=T, showWarnings=F )

  loc.bottom = file.path( basedir, "basedata", "bottom"  )
  dir.create( loc.bottom, recursive=T, showWarnings=F )


  # OSD data series stmv_variables of interest


  if ( DS == "osd.rawdata" ) {
    # simple loading of annual data files
    out = NULL
    for ( y in yr ) {
      # print (y)
      fn = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
      if (file.exists ( fn ) ) {
        load(fn)
        out = rbind( out, X )
      }
    }
    return ( out )
  }


  if ( DS=="osd.rawdata.1910_2008" ) {
    fn.all = list.files( path=loc.archive, pattern="osd.clim.*.gz", full.names=T)
    X = NULL
    varlist = c("DEPTH","PRESSURE","CRUISE_DATE","LATITUDE" ,"LONGITUDE" ,"TEMPERATURE","SALINITY" ,"SIGMAT" )
    varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
    for (fn in fn.all) {
      f = read.csv( gzfile(fn), header=T, as.is=T, sep=",", na.strings="9999")
      f = f[,varlist]
      fyears = as.numeric( matrix( unlist( strsplit( f$CRUISE_DATE, "/" ) ), ncol=3, byrow=T) [,3] )
      years = sort( unique( fyears ))
      for (yrs in years) {
        fn.out = file.path( loc.basedata,  paste( "osd.rawdata", yrs, "rdata", sep=".") )
        print( paste(yrs, ":", fn.out) )
        X = f[ which( fyears == yrs) ,]
        names(X) = tolower( names(X) )
        X$date = lubridate::dmy( X$cruise_date )   # default is UTC ... need to confirm-- no time records .. assume utc
        X = X[ , varstosave ]
        save( X, file=fn.out, compress=T)
      }
    }
  }

  # ----------------------

  if (DS=="osd.rawdata.2008_2016" ) {
    ## this is a data dump directly from Roger Pettipas for 2008 to 2016
    ## overwrites the 2008 data from osd.rawdata_1910_2008
    varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
    fndata = file.path( loc.archive, "Data_2008-2016.csv.xz" )
    XX = read.csv( file=xzfile(fndata), header=FALSE, skip=2 , stringsAsFactors=FALSE, na.strings="9999" )
    header = c("MissionID", "Latitude", "Longitude", "Year", "Month", "Day", "Hour", "Minute", "Pressure", "Temperature", "Salinity", "SigmaT" ,"StationID" )
    names(XX) = tolower( header )
    XX$depth = decibar2depth ( P=XX$pressure, lat=XX$latitude )
    if (!exists( "sigmat", XX))  XX$sigmat = XX$sigma.t  # naming is variable
    XX$date_string = paste( XX$year, XX$month, XX$day, sep="-" )
    XX$date = lubridate::ymd( XX$date_string )   # default is UTC ... need to confirm
    message ( "improper dates on occasion .. they are not an issue as only valid dates are retained .." )
    yrs = sort( unique( XX$year) )
    for ( y in yrs ) {
      print (y)
      fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
      ii = which ( XX$year == y )
      if (length(ii) > 1) {
        X= XX[ ii, varstosave ]
        save( X, file=fn.out, compress=T)
      }
    }
  }


  # ----------------------


  if (DS=="osd.rawdata.annual" ) {
    ## this is a data dump directly from Roger Pettipas for 2016 and on
    varstosave = c( "depth", "pressure", "latitude" ,"longitude" ,"temperature" ,"salinity" ,"sigmat", "date" )
    for ( y in yr ) {
      print (y)
      fndata = file.path( loc.archive, paste( "Data_", y, ".csv.xz", sep="" ) )
      fn.out = file.path( loc.basedata, paste( "osd.rawdata", y, "rdata", sep=".") )
      X = read.csv( file=xzfile(fndata), skip=2, stringsAsFactors=FALSE, na.strings="9999" )
      # insert Header :
      header = c("MissionID", "Latitude", "Longitude", "Year", "Month", "Day", "Hour", "Minute", "Pressure", "Temperature", "Salinity", "SigmaT" ,"StationID" )
      names(X) = tolower( header )
      X$depth = decibar2depth ( P=X$pressure, lat=X$latitude )
      if (!exists( "sigmat", X))  X$sigmat = X$sigma.t  # naming is variable
      X$date_string = paste( X$year, X$month, X$day, sep="-" )
      X$date = lubridate::ymd( X$date_string )   # default is UTC ... need to confirm
      X= X[, varstosave ]
      save( X, file=fn.out, compress=T)
    }
  }

  # ----------------------

  if (DS=="USSurvey_NEFSC") {
    # data dump supplied by Adam Cook .. assumed to tbe bottom temperatures from their surveys in Gulf of Maine area?
    fn = file.path( project.datadirectory("aegis", "temperature"), "archive", "NEFSCTemps_formatted.rdata" )
    if (!is.null(yr)) {
      if (file.exists(fn)) {
        load(fn)
        i = which( lubridate::year( ne$timestamp) %in% yr )
        out = NULL
        if (length(i) > 0) out = ne[i,]
        return(out)
      }
    }
    # else assume a re-assimilation of data
    ne = NULL
    fn_input = file.path( project.datadirectory("aegis", "temperature"), "archive", "NEFSCTemps.rdata" )
    if (file.exists(fn_input)) load(fn_input)
    ne$id = paste(ne$plon, ne$plat, lubridate::date( ne$timestamp), sep="~" )
    ne$salinity = NA
    ne$oxyml = NA
    ne$sigmat = NA
    ne$date = ne$timestamp
    ne$yr = lubridate::year( ne$timestamp )
    ne$dyear = lubridate::decimal_date( ne$timestamp ) - ne$yr
    ne = planar2lonlat( ne, proj.type=p$aegis_proj4string_planar_km )  # convert lon lat to coord system of p0
    save( ne, file=fn, compress=TRUE )

    return (fn)
  }

  # ----------------------

  if (DS %in% c("lobster","lobster.redo")) {
    fn_odbc = file.path( project.datadirectory("aegis", "temperature"), "archive", "FSRStempdata.rdata" )
    fn = file.path( project.datadirectory("aegis", "temperature"), "archive", "FSRStempdata_formatted.rdata" )



    if (DS == "lobster.redo"){
       # require(RODBC)
       #  con = odbcConnect(oracle.server , uid=oracle.lobster.username, pwd=oracle.lobster.password, believeNRows=F) # believeNRows=F required for oracle db's
       #  fsrs = sqlQuery(con, "select * from fsrs_lobster.FSRS_LOBSTER_VW")
       #  odbcClose(con)  

        require(ROracle)
        con=ROracle::dbConnect(DBI::dbDriver("Oracle"),dbname=lobster.oracle.server , username=oracle.lobster.username, password=oracle.lobster.password, believeNRows=F)
        fsrs=ROracle::dbGetQuery(con, "select * from fsrs_lobster.FSRS_LOBSTER_VW")
        ROracle::dbDisconnect(con)

        fsrs$SYEAR = fsrs$HAUL_YEAR # add season-year identifier
        fsrs$HAUL_DATE = as.Date(fsrs$HAUL_DATE)
        fsrs$SYEAR[fsrs$LFA%in%c("33","34")] = as.numeric(substr(fsrs$S_LABEL[fsrs$LFA%in%c("33","34")],6,9)) # add season-year identifier
        fsrsT =  subset(fsrs,TEMP>-90) #remove no temp data
        fsrsT$Dloc = paste(fsrsT$HAUL_DATE,fsrsT$LATITUDE,fsrsT$LONGITUDE)
        fsrsT = subset(fsrsT,!duplicated(Dloc)) #remove duplicate date-locations
        save( fsrsT, file=fn_odbc, compress=T)
    }

    # data dump of above supplied by Brad Hubley (2017) of nearshore lobster trap temperatures (sourced originally from FSRS) and converted into *** daily means ***
    if (!is.null(yr)) {
      if (file.exists(fn)) {
        load(fn)
        i = which( lubridate::year( lob$timestamp) %in% yr )
        out = NULL
        if (length(i) > 0) out = lob[i,]
        return(out)
      } else {
        message( "Data file not found, regenerating ... you will need to re-run this extraction")
      }
    }

    if (file.exists(fn_odbc)) load(fn_odbc)
    lob = NULL
    lob = fsrsT
    names(lob) = tolower( names(lob))

    lon = round( lob$longitude / 100)
    lat = round( lob$latitude / 100)

    potential.errors = NULL
    i =  which( (lob$longitude - (lon*100) ) / 60 < -1)
    j =  which( (lob$latitude - (lat*100) ) / 60 > 1)
    potential.errors = unique( c( i, j  ) )
    lob = lob[ -potential.errors , ]

    lob$lon = lob$long_dd
    lob$lat = lob$lat_dd

    lob = lonlat2planar( lob, proj.type=p$aegis_proj4string_planar_km )

    lob$timestamp = lob$haul_date
    lob$id = paste(lob$plon, lob$plat, lubridate::date( lob$timestamp), sep="~" )
    lob$salinity = NA
    lob$oxyml = NA
    lob$sigmat = NA
    lob$date = lob$timestamp
    lob$yr = lubridate::year( lob$timestamp )
    lob$dyear = lubridate::decimal_date( lob$timestamp ) - lob$yr

    lob$t =lob$temp
    lob$z = lob$depth

    keep = c("z", "lon", "lat", "timestamp", "id", "salinity", "oxyml", "t", "sigmat", "date", "yr", "dyear" )
    lob = lob[, keep]
    save( lob, file=fn, compress=TRUE )
    return (fn)
  }


  # ----------------------


  if (DS %in% c("misc","misc.redo")) {
    # mostly scallop data right now...
    fn_datadump = file.path( project.datadirectory("aegis", "temperature"), "archive", "bottom_temps_survey.csv" )
    fn = file.path( project.datadirectory("aegis", "temperature"), "archive", "bottom_temps_survey.rdata" )

    if (!is.null(yr)) {
      if (file.exists(fn)) {
        load(fn)
        i = which( lubridate::year( misc$timestamp) %in% yr )
        out = NULL
        if (length(i) > 0) out = misc[i,]
        return(out)
      } else {
        message( "Data file not found, regenerating ... you will need to re-run this extraction")
      }
    }

    misc = NULL
    if (file.exists(fn_datadump)) misc = read.csv(fn_datadump)
    names(misc) = tolower( names(misc))

    misc$lon = misc$longitude
    misc$lat = misc$latitude

    i = which( misc$lon < -90)  # deg-min
    misc$lon[i] = trunc(misc$lon[i]/100) + round((misc$lon[i]/100 - trunc(misc$lon[i]/100 ))/60 * 100, 6)

    i = which( misc$lat > 90)  # deg-min
    misc$lat[i] = trunc(misc$lat[i]/100) + round((misc$lat[i]/100 - trunc(misc$lat[i]/100 ))/60 * 100, 6)

    misc = lonlat2planar( misc, proj.type=p$aegis_proj4string_planar_km )
    misc$timestamp = lubridate::dmy( as.character(misc$date ) )
    misc$id = paste(misc$plon, misc$plat, lubridate::date( misc$timestamp), sep="~" )
    misc$salinity = NA
    misc$oxyml = NA
    misc$sigmat = NA
    misc$date = misc$timestamp
    misc$yr = lubridate::year( misc$timestamp )
    misc$dyear = lubridate::decimal_date( misc$timestamp ) - misc$yr

    misc$t =misc$temperature
    misc$z = NA  # no data

    keep = c("z", "lon", "lat", "timestamp", "id", "salinity", "oxyml", "t", "sigmat", "date", "yr", "dyear" )
    misc = misc[, keep]
    save( misc, file=fn, compress=TRUE )
    return (fn)
  }


  # ----------------

  if ( DS %in% c("ODF_ARCHIVE", "ODF_ARCHIVE.redo") ) {

    loc = file.path( project.datadirectory("aegis", "temperature"), "data" )

    DataDumpFromWindows = F
    if ( DataDumpFromWindows ) {
      loc = file.path("C:", "datadump")
    }
    dir.create( path=loc, recursive=T, showWarnings=F )

    fn.root =  file.path( loc, "ODF_ARCHIVE" )
    dir.create( fn.root, recursive = TRUE, showWarnings = FALSE  )

    out = NULL
    if ( is.null(DS) | DS=="ODF_ARCHIVE" ) {
      fl = list.files( path=fn.root, pattern="*.rdata", full.names=T )
        for ( fny in fl ) {
        load (fny)
        out = rbind( out, odfdat )
      }
      return (out)
    }

    con = ROracle::dbConnect( DBI::dbDriver("Oracle"), username=oracle.personal.user, password=oracle.personal.password, dbname="PTRAN" )
    cruises   = ROracle::dbGetQuery(con, "select * from ODF_ARCHIVE.ODF_CRUISE_EVENT" )

    for ( y in yr ) {
      fny = file.path( fn.root, paste( y, "rdata", sep="."))
      odfdat = ROracle::dbGetQuery( con,  paste(
      " select * " ,
      " from ODF_ARCHIVE.ODF_CRUISE_EVENT i, ODF_ARCHIVE.ODF_DATA j " ,
      " where i.CRUISE_EVENT_ID(+)=j.DATA_VAL_ID ",
      " and EXTRACT(YEAR from start_date_time) =", y
      ) )

      names(odfdat) =  tolower( names(odfdat) )
      print(fny)
      save(odfdat, file=fny, compress=T)
      gc()  # garbage collection
      print(y)
    }

    ROracle::dbDisconnect(connect)

    return (fn.root)

  }

  # ----------------

  if (DS %in% c( "osd.profiles.annual.redo", "osd.profiles.annual" ) ) {
    # read in annual depth profiles then extract bottom temperatures

    if (DS=="osd.profiles.annual") {
      fn = file.path(  loc.profile, paste("depthprofiles", yr, "rdata", sep="."))
      Y = NULL
      if (file.exists( fn) ) load (fn )
      return(Y)
    }

    if (is.null(yr)) yr = p$yrs


    # # bring in snow crab, groundfish and OSD data ...
    # set = bio.snowcrab::snowcrab.db( DS="setInitial" )
    # mlu = bio.snowcrab::minilog.db( DS="set.minilog.lookuptable" )
    # slu = bio.snowcrab::seabird.db( DS="set.seabird.lookuptable" )
    # set = merge( set, mlu, by= c("trip", "set"), all.x=TRUE, all.y=FALSE )
    # set = merge( set, slu, by= c("trip", "set"), all.x=TRUE, all.y=FALSE )
    # slu = mlu = NULL

    # set$longitude =set$lon
    # set$latitude = set$lat
    # set$oxyml = NA
    # set$salinity = NA
    # set$sigmat = NA

    # set = set[ ,c("minilog_uid", "seabird_uid", "longitude", "latitude", "oxyml", "salinity", "sigmat" ) ]

    message ("Starting extraction of profiles: ")
    parallel_run(
      p=p,
      runindex=list( yrs=yr ),
      loc.profile=loc.profile,
      FUNC= function( ip=NULL, p, loc.profile ) {
        if (exists( "libs", p)) RLibrary( p$libs )
        if (is.null(ip)) ip = 1:p$nruns

        for (iy in ip) {
          yt = p$runs[iy, "yrs"]

          Ydummy = temperature_db( DS="osd.rawdata", yr=2000, p=p ) [1,]  # dummy entry using year=2000
          Ydummy$yr = NA
          Ydummy$dyear = 0.5
          Ydummy$id =  "dummy"
          Ydummy$depth = -1
          Ydummy$oxyml = NA

          Y =  temperature_db( DS="osd.rawdata", yr=yt, p=p )
            if ( is.null(Y) ) {
              Y = Ydummy
              Y$yr = yt
            } else {
              Y$yr = yt
              Y$dyear = lubridate::decimal_date( Y$date ) - Y$yr
              Y$id =  paste( round(Y$longitude,4), round(Y$latitude,4), as.character(Y$data), sep="~" )
              Y$depth = decibar2depth ( P=Y$pressure, lat=Y$latitude )
              Y$oxyml = NA
              # next should not be necessary .. but just in case the osd data types get altered
              Y$temperature = as.numeric(Y$temperature )
              Y$salinity= as.numeric(Y$salinity)
              Y$sigmat = as.numeric(Y$sigmat)
            }

          Y$pressure = NULL

          oo = which( Y$id == "dummy" )
          if (length(oo) > 0 ) Y = Y[ -oo, ]

          if ( is.null( nrow(Y) ) ) next()
          if ( nrow(Y) < 5 ) next()

          if ( is.null(Y) ) next()

          iiY = which(duplicated(Y))
          if (length(iiY)>0) Y = Y [ -iiY, ]

          bad = which( Y$temperature < -5 | Y$temperature > 30 )
          if (length(bad)>0) Y=Y[-bad,]

          fn = file.path( loc.profile, paste("depthprofiles", yt, "rdata", sep="."))
          print( fn )
          save( Y, file=fn, compress=T )
        }
      }
    )
    return ("Completed")
  }


  # ----------------

  if (DS %in% c( "bottom.annual.rawdata", "bottom.annual.rawdata.redo" ) ) {
    # extract bottom temperatures and save annual time slice
   
    if (DS=="bottom.annual.rawdata") {
      fn = file.path( loc.bottomdatabase, paste("bottom", yr, "rdata", sep="."))
      Z = NULL
      if (file.exists(fn) ) load (fn )
      return(Z)
    }

    if (is.null(yr)) yr = p$yrs

      con = ROracle::dbConnect( DBI::dbDriver("Oracle"),
        username = oracle.snowcrab.user,
        password = oracle.snowcrab.password,
        dbname = oracle.snowcrab.server
      )

      res = ROracle::dbSendQuery( con, "ALTER SESSION SET NLS_DATE_FORMAT = 'YYYY-MM-DD'")
      res = ROracle::dbSendQuery( con, "ALTER SESSION SET NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SSXFF'")
      res = ROracle::dbSendQuery( con, "ALTER SESSION SET NLS_TIMESTAMP_TZ_FORMAT = 'YYYY-MM-DD HH24:MI:SSXFF TZR'")
      # res = ROracle::dbReadTable( con, "SC_TEMP_MERGE")
      # str(res):
      #   data.frame':	2914559 obs. of  6 variables:
      # $ PROJECT: chr  "Vemco" "Vemco" "Vemco" "Vemco" ...
      # $ T_DATE : POSIXct, format: "2010-06-24 21:27:36" "2010-06-24 21:42:36" "2010-06-24 21:57:36" "2010-06-24 22:12:36" ...
      # $ LAT_DD : num  43.7 43.7 43.7 43.7 43.7 ...
      # $ LON_DD : num  -65.8 -65.8 -65.8 -65.8 -65.8 ...
      # $ T_UID  : chr  "AAA090-1" "AAA090-1" "AAA090-1" "AAA090-1" ...
      # $ TEMP   : num  12.7 12.8 12.8 12.6 12.6 12.5 12.5 12.5 12.5 12.5 ...

      ythreshold = 1970 

      for ( yt in yr ) {
        if (yt < ythreshold) {
          message( "Warning: ",  yt, "is not in this database ... skipping" )
          next()
        }
        Z = NULL
        Z = ROracle::dbGetQuery( con,  paste(
          " select * " ,
          " from SC_TEMP_MERGE " ,
          " where EXTRACT(YEAR from SC_TEMP_MERGE.T_DATE) =", yt
        ) )

        if (nrow(Z) > 0 ) {
          if (ncol(Z) == 6) names(Z) = c("project", "date", "lat", "lon", "t_uid", "t" )
          if (ncol(Z) == 7) names(Z) = c("project", "date", "lat", "lon", "t_uid", "t", "z" )
          Z$yr = yt
          Z$dyear = lubridate::decimal_date( Z$date ) - Z$yr
        } else {
          Z = NULL
        }
        if (!is.null(Z)) {
          if ( nrow(Z) > 0  ) {
            fn = file.path(  loc.bottomdatabase, paste("bottom", yt, "rdata", sep="."))
            print (fn)
            save( Z, file=fn, compress=T)
          }
        }
      }

  }


  if (DS %in% c( "bottom.annual", "bottom.annual.redo" ) ) {
    # extract bottom temperatures and save annual time slice

    if (DS=="bottom.annual") {
      fn = file.path( loc.bottom, paste("bottom", yr, "rdata", sep="."))
      Z = NULL
      if (file.exists(fn) ) load (fn )
      return(Z)
    }

    # probably want to change to DT : todo

    output_vars = c( "project", "date", "lon", "lat", "t", "z", "dyear", "yr", "t_uid" )

    BS = bathymetry_db ( p=bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="core"  ), DS="aggregated_data" )  # raw data
    BS_map = array_map( "xy->1", BS[,c("plon","plat")], gridparams=p$gridparams )

    for ( yt in yr ) {
      message( "bottom annual year ", yt)
      Z = NULL
      TDB = temperature_db( p=p, DS="bottom.annual.rawdata", yr=yt )

      if (!is.null(TDB)) {
        if (nrow(TDB) > 0 ) {
          igood = which( TDB$lon >= p$corners$lon[1] & TDB$lon <= p$corners$lon[2]
                      &  TDB$lat >= p$corners$lat[1] & TDB$lat <= p$corners$lat[2] )
          if (length( igood) > 0 ) {
            TDB = TDB[igood, ]
            bad = which( TDB$t < -5 | TDB$t > 30 )
            if (length(bad) > 0) TDB=TDB[-bad,]
            # add approximate depth for filtering.. high resolution space
            TDB = lonlat2planar( TDB, p$aegis_proj4string_planar_km )
            TDB_map = NULL
            if (exists("z", TDB)) {
              im = which(!is.finite( TDB$z ))
              TDB_map = array_map( "xy->1", TDB[im, c("plon","plat")], gridparams=p$gridparams )
              if (length( im) > 0) TDB$z[im] = BS[ match( TDB_map, BS_map ), "z.mean" ]
            } else {
              TDB_map = array_map( "xy->1", TDB[,c("plon","plat")], gridparams=p$gridparams )
              TDB$z = BS[ match( TDB_map, BS_map ), "z.mean" ]
            }
            TDB_map = NULL
            TDB = TDB[ is.finite(TDB$z), ]
          } else {
            TDB = NULL
          }
        }
      }

      bottom = NULL 
      profile = NULL
      profile = temperature_db( DS="osd.profiles.annual", yr=yt, p=p )
 
      if (!is.null(profile)) {
        igood = which( profile$lon >= p$corners$lon[1] & profile$lon <= p$corners$lon[2]
                    &  profile$lat >= p$corners$lat[1] & profile$lat <= p$corners$lat[2] )
        if (length( igood) > 0 ) {
          profile = profile[igood, ]
          names(profile)[which(names(profile) == "longitude" ) ] = "lon"
          names(profile)[which(names(profile) == "latitude" ) ]  = "lat"
          names(profile)[which(names(profile) == "temperature" ) ]  = "t"
          names(profile)[which(names(profile) == "depth" ) ]  = "z"

          # Discretize here in time and space to manageable blocks

          profile = lonlat2planar( profile, proj.type=p$aegis_proj4string_planar_km )
          profile = profile[ which( is.finite( profile$plon + profile$plat + profile$t + profile$z ) ) , ]

          # don't need year as this is a yearly breakdown but just to be clear ..
          profile$id =  paste(
            trunc(profile$plon/p$inputdata_spatial_discretization_planar_km + 1) * p$inputdata_spatial_discretization_planar_km,
            trunc(profile$plat/p$inputdata_spatial_discretization_planar_km + 1) * p$inputdata_spatial_discretization_planar_km,
            paste(profile$yr, cut( profile$dyear, breaks=p$dyear_discretization_rawdata, include.lowest=T, ordered_result=TRUE ), sep="_" ),
            sep="~"
          )
          ids =  sort( unique( profile$id ) )
          nids = length(ids)
          bottom = profile[1:nids,]
          bottom$id = NA

          for (i in 1:nids ) {
            W = profile[which( profile$id == ids[i] ), ]
            jj = which( is.finite( W$z ) )
            if (length(jj) ==0 ) next()
            Wmax = max( W$z, na.rm=T ) - 5  # accept any depth within 5 m of the maximum depth
            kk =  which( W$z >= Wmax )
            bottom[i,] = W[ which.max( W$z ) , ]
            bottom[i,"t"] = median( W[kk,"t"] , na.rm=T )
            # bottom[i,"salinity"] = median( W[kk,"salinity"] , na.rm=T )
            # bottom[i,"sigmat"] = median( W[kk,"sigmat"] , na.rm=T )
            # bottom[i,"oxyml"] = median( W[kk,"oxyml"] , na.rm=T )
          }
          withdata = which(!is.na(bottom$id))
          if (length(withdata) > 0) {
            bottom = bottom[ withdata, ]
            bottom$date = as.Date( bottom$date ) # strip out time of day information
            bottom$dyear = lubridate::decimal_date( bottom$date ) - bottom$yr
          }
          bottom$project = "OSD"
          names(bottom)[which(names(bottom) == "id" ) ]  = "t_uid"

        }
      }

      if ( !is.null(TDB)) if ( nrow(TDB) > 0  )   Z = rbind( Z, TDB[ , output_vars] )
      TDB = NULL

      if ( !is.null(bottom) ) Z = rbind( Z, bottom[ , output_vars ] )

      if ( is.null( nrow(Z) ) ) next()
      if ( nrow(Z) < 1 ) next()
      if ( is.null(Z) ) next()

      igood = which( Z$t >= -3 & Z$t <= 30 )  ## 30 is a bit high but in case some shallow data
      if (length( igood) == 0 ) next()

      Z = Z[igood, ]


      ## ensure that inside each grid/time point
      ## that there is only one point estimate .. taking medians
      # vars = c("z", "t", "salinity", "sigmat", "oxyml")
      vars = c("z", "t")

      locs = Z[, c("lon", "lat")]
      locs = lonlat2planar( locs, proj.type=p$aegis_proj4string_planar_km )
      locs$plon = trunc(locs$plon / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km
      locs$plat = trunc(locs$plat / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km

      nw = 1 / p$inputdata_temporal_discretization_yr
      dyears_cuts = (c(1:nw)-1) / nw # intervals of decimal years... fractional year breaks
      dyears_cuts = c(dyears_cuts, dyears_cuts[length(dyears_cuts)]+ diff(dyears_cuts)[1] )

      Z_st_id = paste(
        locs$plon,
        locs$plat,
        as.numeric( cut( (lubridate::decimal_date( Z$date ) - yt), breaks=dyears_cuts, include.lowest=T, ordered_result=TRUE ) ),
        sep ="."
      )

      o = which( ( duplicated( Z_st_id )) )
      if (length(o)>0) {
        dupids = unique( Z_st_id[o] )
        for ( dd in dupids ) {
          e = which( Z_st_id == dd )
          keep = e[1]
          drop = e[-1]
          for (v in vars) Z[keep, v] = median( Z[e,v], na.rm=TRUE )
          Z_st_id[drop] = NA  # flag for deletion
        }
        Z = Z[ -which( is.na( Z_st_id)) ,]
      }
      Z_st_id = NULL
      fn = file.path( loc.bottom, paste("bottom", yt, "rdata", sep="."))
      print (fn)
      save( Z, file=fn, compress=T)
    }

    return ("Completed")
  }


  # -----------------------


  if (DS %in% c( "bottom.all", "bottom.all.redo" ) ) {
    # collect all bottom temperatures (1990 to present)

    fbAll = file.path( loc.bottom, "bottom.all.rdata" )
    if (DS=="bottom.all") {
      O = NULL
      if (file.exists(fbAll) ) load (fbAll)
      return(O)
    }
    O = NULL
    

    for ( y in yr ) {
      o = temperature_db( p=p, DS="bottom.annual", yr=y )
      message( "Year: ", y, " --  depth ranges (m): ", paste0( range(o$z, na.rm=T), sep="  ") )
      if (!is.null(o)) O = rbind(O, o)
    }

    # depths look to be problematic? : reject and use stmv solution:
    pBD = bathymetry_parameters(  spatial_domain=p$spatial_domain, project_class="stmv" )  # full default
    LU = bathymetry_db( p=pBD, DS="baseline", varnames="all" )
    LU_map = array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams )
    O = lonlat2planar(O, p$aegis_proj4string_planar_km)
    M_map  = array_map( "xy->1", O[, c("plon","plat")], gridparams=p$gridparams )
    iML = match( M_map, LU_map )
    O[, "z"] = LU[ iML, "z" ]

    save(O, file=fbAll, compress=TRUE)
    return(fbAll)
  }




  # -----------------------



  if ( DS=="aggregated_data") {

    fn = file.path( loc.bottom, paste( "temperature", "aggregated_data", round(p$inputdata_spatial_discretization_planar_km, 6), round(p$inputdata_temporal_discretization_yr, 6), "rdata", sep=".") )
    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        return( M )
      }
    }
    warning( "Generating aggregated data ... ")

    M = temperature_db( p=p, DS="bottom.all"  )
    M[, p$variabletomodel] = M$t

    M = M[ which(M$yr %in% p$yrs), ]
    M$tiyr = lubridate::decimal_date ( M$date )

    # globally remove all unrealistic data
    keep = which( M[,p$variabletomodel] >= -3 & M[,p$variabletomodel] <= 25 ) # hard limits
    if (length(keep) > 0 ) M = M[ keep, ]

    # p$quantile_bounds = c(0.0005, 0.9995)
    if (exists("quantile_bounds", p)) {
      TR = quantile(M[,p$variabletomodel], probs=p$quantile_bounds, na.rm=TRUE ) # this was -1.7, 21.8 in 2015
      keep = which( M[,p$variabletomodel] >=  TR[1] & M[,p$variabletomodel] <=  TR[2] )
      if (length(keep) > 0 ) M = M[ keep, ]
       # this was -1.7, 21.8 in 2015
    }

    keep = which( M$z >=  5 ) # ignore very shallow areas ..
    if (length(keep) > 0 ) M = M[ keep, ]

    M = lonlat2planar( M, p$aegis_proj4string_planar_km)
    # in case plon/plats are from an alternate projection  .. as there are multiple data sources

    M$plon = trunc(M$plon / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km
    M$plat = trunc(M$plat / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km

    M$dyear = M$tiyr - M$yr
    i = which( M$dyear < 0 )  
    if (length(i) > 0) {
      message( "Data with time of year 'dyear' that are negative valued: setting to zero" )
      print( M[i,] )
      message( "Mostly OSD data and likely due to timestamping/roundoff issues. Might want to check these." )
      M$dyear[i] = 0.001
    }

    j = which( M$dyear > 1 )  
    if (length(j) > 0) {
      message( "Data with time of year 'dyear' that are greater than 1: setting to 1" )
      print( M[j,] )
      message( "Might want to check these." )
      M$dyear[j] = 0.999
    }

    M$dyear = discretize_data( M$dyear, seq(0, 1, by=p$inputdata_temporal_discretization_yr), digits=6 )

    k = which( !is.finite(M$dyear +M$tiyr) )  
    if (length(j) > 0) {
      message("Data with timestamp issues found. Dropping these:")
      print( M[k,] )
      M = M[-k,]
    }

    setDT(M)
    M$t = M[[p$variabletomodel]]
    M = M[, .( mean=mean(t, na.rm=TRUE), sd=sd(t, na.rm=TRUE), n=length(which(is.finite(t))), meanz=mean(z, na.rm=TRUE), sdz=sd(z, na.rm=TRUE), nz=length(which(is.finite(z))) ), by=list(plon, plat, yr, dyear) ]
    colnames(M) = c( "plon", "plat", "yr", "dyear", paste( p$variabletomodel, c("mean", "sd", "n"), sep="."), paste( "z", c("mean", "sd", "n"), sep=".") )
    setDF(M)

    M = planar2lonlat( M, p$aegis_proj4string_planar_km)

    save( M, file=fn, compress=TRUE )
    return( M )
  }


  # -----------------------

  if ( DS=="areal_units_input" ) {

    
    outdir = file.path( p$data_root, "modelled", p$carstm_model_label ) 
    fn = file.path( outdir, "areal_units_input.rdata"  )
    if ( !file.exists(outdir)) dir.create( outdir, recursive=TRUE, showWarnings=FALSE )

    xydata = NULL
    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        return( xydata )
      }
    }
    xydata = temperature_db( p=p, DS="aggregated_data"  )  #
    names(xydata)[which(names(xydata)=="z.mean" )] = "z"
    xydata = xydata[ geo_subset( spatial_domain=p$spatial_domain, Z=xydata ) , ] # need to be careful with extrapolation ...  filter depths

    keep = which( xydata$lon < -54 & xydata$lon > -71 & xydata$lat < 49 & xydata$lat > 41 )

    xydata =xydata[ keep, ]

    # drop a corner far offshore
    x0 = c(40, -60 )
    x1 = c(43, -55 )
    m = ( x0[2] - x1[2] ) / ( x0[1] - x1[1] )
    b = x0[2] - m *x0[1]   # -60 = m * 40 + b
    i = which( xydata$lon > m*xydata$lat + b )
    xydata = xydata[ -i, ]

    # drop a corner Gulf of St Lawrence
    x0 = c(46, -71 )
    x1 = c(49, -66 )
    m = ( x0[2] - x1[2] ) / ( x0[1] - x1[1] )
    b = x0[2] - m *x0[1]   # -60 = m * 40 + b
    i = which( xydata$lon < m*xydata$lat + b )

    xydata = xydata[ -i, ]


    # drop a corner NFLD
    x0 = c(48, -54 )
    x1 = c(49, -58.5 )
    m = ( x0[2] - x1[2] ) / ( x0[1] - x1[1] )
    b = x0[2] - m *x0[1]   # -60 = m * 40 + b
    i = which( xydata$lon > m*xydata$lat + b )

    xydata = xydata[ -i, ]

    xydata = xydata[ which(xydata$z  < 2500) , ]
    xydata = xydata[ which(xydata$z  > 5) , ]
    xydata = xydata[ , c("lon", "lat", "yr" )]

    # could filter earlier for speed but here it is more compact  
    if (exists("carstm_input_time_limit", p))  xydata = xydata[ which( xydata$yr >= p$carstm_input_time_limit), ]   

    save(xydata, file=fn, compress=TRUE )
    return( xydata )
   }

  # -----------------------


  if ( DS=="carstm_inputs") {

    # prediction surface
    crs_lonlat = st_crs(projection_proj4string("lonlat_wgs84"))
    if (is.null(sppoly)) sppoly = areal_units( p=p )  # will redo if not found
    sppoly = st_transform(sppoly, crs=crs_lonlat )
    areal_units_fn = attributes(sppoly)[["areal_units_fn"]]

    fn = file.path( p$modeldir, p$carstm_model_label, paste("carstm_inputs", areal_units_fn, sep="_") )
    if (p$carstm_inputs_prefilter =="rawdata") {
      fn = file.path( p$modeldir, p$carstm_model_label, paste("carstm_inputs_rawdata", areal_units_fn, sep="_") )
    }

    # inputs are shared across various secneario using the same polys
    #.. store at the modeldir level as default
    outputdir = dirname( fn )
    if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

    M = NULL
    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        M = M[ which( M$z < 2500) , ]
        M = M[ which( M$z > 5 ) , ]
        M = M[ which( M$yr %in% p$yrs), ]
        return( M )
      }
    }

    # do this immediately to reduce storage for sppoly (before adding other variables)

    if (p$carstm_inputs_prefilter == "aggregated") {

      M = temperature_db( p=p, DS="aggregated_data"  )
      names(M)[which(names(M)==paste(p$variabletomodel, "mean", sep=".") )] = p$variabletomodel
      # reduce size
      M = M[ which( M$lon > p$corners$lon[1] & M$lon < p$corners$lon[2]  & M$lat > p$corners$lat[1] & M$lat < p$corners$lat[2] ), ]
      # M = M[ which(M$yr %in% p$yrs), ]  # note do not filter by year .. use all available data to estimate params
      # M = lonlat2planar( M, p$aegis_proj4string_planar_km) # in case plon/plats are from an alternate projection  .. as there are multiple data sources
      M$tiyr = M$yr + M$dyear
   
   } else if (p$carstm_inputs_prefilter =="sampled") {
   
      require(data.table)

      M = temperature_db( p=p, DS="bottom.all"  )
      # M = M[ which(M$yr %in% p$yrs), ]   # note do not filter by year .. use all available data to estimate params
      names(M)[which(names(M)=="t")] = p$variabletomodel

      M = M[ which( !duplicated(M)), ]
      M = M[ which( M$lon > p$corners$lon[1] & M$lon < p$corners$lon[2]  & M$lat > p$corners$lat[1] & M$lat < p$corners$lat[2] ), ]
    # levelplot( eval(paste(p$variabletomodel, "mean", sep="."))~plon+plat, data=M, aspect="iso")

  # thin data a bit ... remove potential duplicates and robustify
      M = lonlat2planar( M, proj.type=p$aegis_proj4string_planar_km )  # first ensure correct projection

      M$plon = trunc(M$plon / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km
      M$plat = trunc(M$plat / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km
  
      setDT(M)
      M = M[,.SD[sample(.N, min(.N, p$carstm_inputs_prefilter_n))], by =list(plon, plat) ]  # compact, might be slightly slower
      # M = M[ M[, sample(.N, min(.N, p$carstm_inputs_prefilter_n) ), by=list(plon, plat)], .SD[i.V1], on=list(plon, plat), by=.EACHI]  # faster .. just a bit
      setDF(M)

    } else {

      M = temperature_db( p=p, DS="bottom.all"  )
      # M = M[ which(M$yr %in% p$yrs), ]   # note do not filter by year .. use all available data to estimate params
      names(M)[which(names(M)=="t")] = p$variabletomodel

    }

    attr( M, "proj4string_planar" ) =  p$aegis_proj4string_planar_km
    attr( M, "proj4string_lonlat" ) =  projection_proj4string("lonlat_wgs84")
    M = lonlat2planar( M, p$aegis_proj4string_planar_km) # in case plon/plats are from an alternate projection  .. as there are multiple data sources

    # globally remove all unrealistic data
    keep = which( M[,p$variabletomodel] >= -3 & M[,p$variabletomodel] <= 25 ) # hard limits
    if (length(keep) > 0 ) M = M[ keep, ]

    if (p$carstm_inputs_prefilter != "aggregated") {
      # aggregated data already have been truncated ... 
      if (exists("quantile_bounds", p)) {
        TR = quantile(M[,p$variabletomodel], probs=p$quantile_bounds, na.rm=TRUE ) # this was -1.7, 21.8 in 2015
        keep = which( M[,p$variabletomodel] >=  TR[1] & M[,p$variabletomodel] <=  TR[2] )
        if (length(keep) > 0 ) M = M[ keep, ]
        # this was -1.7, 21.8 in 2015
      }
    } 
    
    keep = NULL
    if (exists( "z.mean", M)) {
      if (!exists( "z", M)) {
        names(M)[which(names(M)=="z.mean" )] = "z"
      } 
    }
 
    M$tiyr = M$yr + M$dyear 
 
    M = carstm_prepare_inputdata( p=p, M=M, sppoly=sppoly)

    M$space = match( M$AUID, sppoly$AUID ) # for bym/car .. must be numeric index matching neighbourhood graphs
    M$space_time = M$space  # copy for space_time component (INLA does not like to re-use the same variable in a model formula) 
    M$space_cyclic = M$space # copy for space_time component (INLA does not like to re-use the same variable in a model formula) 

    M$time = match( M$year, p$yrs ) # copy time for space_time component .. for groups, must be numeric index
    M$time_space = M$time    
   
    # as numeric is simpler
    cyclic_levels = p$dyears + diff(p$dyears)[1]/2 

    M$cyclic = match( M$dyri, discretize_data( cyclic_levels, seq( 0, 1, by=0.1 ) ) ) 
    M$cyclic_space = M$cyclic # copy cyclic for space - cyclic component .. for groups, must be numeric index
   

    M = M[ which( M$z < 2500), ]
    M = M[ which( M$z > 5 ), ]

      
    # could filter earlier for speed but here it is more compact  
    if (exists("carstm_input_time_limit", p))  M = M[ which( M$yr >= p$carstm_input_time_limit), ]   

    save( M, file=fn, compress=TRUE )

    return( M )
  }


  # -----------------------


  if (DS=="stmv_inputs") {
    # default output grid
    vars_required = c(p$stmv_variables$LOCS, p$stmv_variables$COV )

    pB = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="default"  )  # default is the "best" performing method

    Bout = bathymetry_db( p=pB, DS="baseline", varnames=vars_required )  # this is a subset of "complete" with depths filtered
    tokeep = which( names(Bout) %in% vars_required )
    toadd = setdiff( 1:length(vars_required),  which( vars_required %in% names(Bout)) )
    Bout = Bout[,tokeep]
    if (length(p$stmv_variables$COV)==1) {
      covs = list( Bout[,p$stmv_variables$COV] )
      names(covs) = p$stmv_variables$COV
      OUT  = list( LOCS = Bout[,p$stmv_variables$LOCS], COV=covs )
    } else {
      OUT  = list( LOCS = Bout[,p$stmv_variables$LOCS], COV=as.list( Bout[,p$stmv_variables$COV] ) )
    }

    B = temperature_db( p=p, DS="aggregated_data"  )
    B$lon = NULL
    B$lat = NULL
    names(B)[which(names(B) == paste(p$variabletomodel, "mean", sep="."))] = p$variabletomodel
    #names(B)[which(names(B) == paste("z", "mean", sep="."))] = "z"

    locsmap = match(
      array_map( "xy->1", B[,c("plon","plat")], gridparams=p$gridparams ),
      array_map( "xy->1", bathymetry_db(p=pB, DS="baseline", project_class="default"), gridparams=p$gridparams ) )

    Bnames = names(B)
    newvars = setdiff(p$stmv_variables$COV, Bnames )
    if (length(newvars) > 0) {
        B = cbind( B,  Bout[locsmap, newvars] )
        names(B) = c( Bnames, newvars )
    }

    varstokeep = unique( c( p$stmv_variables$Y, p$stmv_variables$LOCS, p$stmv_variables$TIME, p$stmv_variables$COV ) )
    B$tiyr = B$yr + B$dyear
    B = B[,varstokeep]
    B = B[ which(is.finite(rowSums(B))), ]
    return (list(input=B, output=OUT))


  }


  # -----------------


  if ( DS %in% c("predictions", "predictions.redo" ) ) {
    # NOTE: the primary interpolated data were already created by stmv.
    # This routine points to this data and also creates
    # subsets of the data where required, determined by "spatial_domain_subareas"

    outdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p$spatial_domain )

    if (DS %in% c("predictions")) {
      P = Pl = Pu = NULL
      fn = file.path( outdir, paste("stmv.prediction", ret,  yr, "rdata", sep=".") )
      if (file.exists(fn) ) load(fn)
      if (ret=="mean") return (P)
      if (ret=="lb") return( Pl)
      if (ret=="ub") return( Pu)
    }

    if (is.null(yr)) yr = p$yrs

    parallel_run(
      p=p,
      runindex=list( yrs=yr ),
      voi=voi,
      FUNC= function( ip=NULL, p=p, voi=voi ) {
        if (exists( "libs", p)) RLibrary( p$libs )
        if (is.null(ip)) ip = 1:p$nruns
        # downscale and warp from p(0) -> p1
        for ( iy in ip ) {
          # print (r)
          yy = p$runs[iy, "yrs"]
          # default domain
          PP0 = stmv_db( p=p, DS="stmv.prediction", yr=yy, ret="mean")
          VV0 = stmv_db( p=p, DS="stmv.prediction", yr=yy, ret="lb")
          WW0 = stmv_db( p=p, DS="stmv.prediction", yr=yy, ret="ub")
          p0 = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="default"  )  # default is the "best" performing method
          L0 = bathymetry_db( p=p0, DS="baseline" )
          L0i = array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
          sreg = setdiff( p$spatial_domain_subareas, p$spatial_domain )
          for ( gr in sreg ) {
            p1 = bathymetry_parameters( spatial_domain=gr, project_class="default"  )  # default is the "best" performing method
            L1 = bathymetry_db( p=p1, DS="baseline" )
            L1i = array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
            L1 = planar2lonlat( L1, proj.type=p1$aegis_proj4string_planar_km )
            L1$plon_1 = L1$plon # store original coords
            L1$plat_1 = L1$plat
            L1 = lonlat2planar( L1, proj.type=p0$aegis_proj4string_planar_km )
            p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres,
              theta=p1$pres/3, xwidth=4*p1$pres, ywidth=4*p1$pres )
            # theta=p1$pres/3 assume at pres most of variance is accounted ... correct if dense pre-intepolated matrices .. if not can be noisy
            P = Pl = Pu = matrix( NA, ncol=p$nw, nrow=nrow(L1) )
            for (iw in 1:p$nw) {
              P[,iw]  = spatial_warp( PP0[,iw], L0, L1, p0, p1, "fast", L0i, L1i )
              Pl[,iw] = spatial_warp( VV0[,iw], L0, L1, p0, p1, "fast", L0i, L1i )
              Pu[,iw] = spatial_warp( WW0[,iw], L0, L1, p0, p1, "fast", L0i, L1i )
            }
            outdir_p1 = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p1$spatial_domain )
            dir.create( outdir_p1, recursive=T, showWarnings=F )
            fn1_sg = file.path( outdir_p1, paste("stmv.prediction.mean",  yy, "rdata", sep=".") )
            fn2_sg = file.path( outdir_p1, paste("stmv.prediction.lb",  yy, "rdata", sep=".") )
            fn3_sg = file.path( outdir_p1, paste("stmv.prediction.ub",  yy, "rdata", sep=".") )
            save( P, file=fn1_sg, compress=T )
            save( Pl, file=fn2_sg, compress=T )
            save( Pu, file=fn3_sg, compress=T )
            print (fn1_sg)
          }
        }
      } # end FUNC
    )
    return ("Completed")

    if (0) {
      aoi = which( PS$z > 5 & PS$z < 3000 & PS$z.range < 500)
      levelplot( log(z) ~ plon + plat, PS[ aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      levelplot( log(t.ar_1) ~ plon + plat, PS[ aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )

      levelplot( log(t.range) ~ plon + plat, PS[ aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      levelplot( Z.rangeSD ~ plon + plat, PS[aoi,], aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    }

  }


  # -----------------


  if (DS %in% c(  "bottom.statistics.annual", "bottom.statistics.annual.redo", "bottom.statistics.climatology", "bottom.degree.days", "bottom.degree.days.climatology" )){

    tstatdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p$spatial_domain )
    dir.create( tstatdir, showWarnings=F, recursive = TRUE )

    if (DS == "bottom.statistics.climatology" ) {
      clim = NULL
      fn.climatology = file.path( tstatdir, paste("bottom.statistics.climatology", p$spatial_domain, "rdata", sep=".") )
      if (file.exists( fn.climatology ) ) load(fn.climatology)
      return ( clim )
    }


    if (DS %in% c("bottom.statistics.annual")) {
      BS = NULL
      fn = file.path( tstatdir, paste("bottom.statistics.annual", p$spatial_domain, ret, "rdata", sep=".") )
      if (file.exists( fn) ) load(fn)
      return ( BS )
    }

    if (DS == "bottom.degree.days.climatology" ) {
      bdd.clim = NULL
      fn.bdd.climatology = file.path( tstatdir, paste("bottom.degree.days.climatology", p$spatial_domain, "rdata", sep=".") )
      if (file.exists( fn.bdd.climatology ) ) load(fn.bdd.climatology)
      return ( bdd.clim )
    }

		if (DS %in% c("bottom.degree.days")) {
      bdd = NULL
      bdd.fn = file.path( tstatdir, paste("bottom.degree.days", p$spatial_domain, "rdata", sep=".") )
      if (file.exists( bdd.fn) ) load(bdd.fn)
      return ( bdd )
    }

    if (is.null(yr)) yr = p$yrs

    grids = unique( c( p$spatial_domain_subareas, p$spatial_domain ) )

        # downscale and warp from p(0) -> p1
        for ( gr in grids ) {
          # print(gr)
          p1 = bathymetry_parameters( spatial_domain=gr, project_class="default"  )  # default is the "best" performing method
          L1 = bathymetry_db(p=p1, DS="baseline")
          tstatdir_p1 = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p1$spatial_domain )

          dir.create( tstatdir_p1, showWarnings=F, recursive = TRUE )

          O = array( NA, dim=c( nrow(L1), p$ny, length(p$bstats)) )
          bdd = array( NA, dim=c( nrow(L1), p$ny, p$nw ), dimnames=list(NULL, p$yrs, p$dyears) )
          for ( iy in 1:p$ny ) {
            y = p$yrs[iy]
      			print ( paste("Year:", y)  )
            P = temperature_db( p=p1, DS="predictions", yr=y, ret="mean"  )
            O[,iy,1] = c(apply( P, 1, mean, na.rm=T))
            O[,iy,2]  = c(apply( P, 1, sd, na.rm=T )) # annual, seasonal mean sums of squares
      			O[,iy,3] = c(apply( temperature_db( p=p1, DS="predictions", yr=y, ret="lb"  ), 1, mean, na.rm=TRUE ) )
            O[,iy,4] = c(apply( temperature_db( p=p1, DS="predictions", yr=y, ret="ub"  ), 1, mean, na.rm=TRUE ))
            O[,iy,5] = O[,iy,4] - O[,iy,3]
            O[,iy,6] = rowSums( P[] ) / p$nw * 365 # total degree days
            bdd[,iy,] = t(apply( P[], 1, cumsum )) / p$nw * 365 # normalized degree days (X 365 for degree days)
            P = NULL
          }

          fn.bdd = file.path( tstatdir_p1, paste("bottom.degree.days", p1$spatial_domain, "rdata", sep=".") )
          save( bdd, file=fn.bdd, compress=TRUE )
          # climatology bdd
          bdd.clim = matrix( NA, nrow=nrow(L1), ncol=p$nw )
          for (si in 1:p$nw) {
            bdd.clim[,si] = rowMeans(bdd[,,si], na.rm=T)
          }
          fn.bdd.climatology = file.path( tstatdir_p1, paste("bottom.degree.days.climatology", p1$spatial_domain, "rdata", sep=".") )
          colnames(bdd.clim) = 1:p$nw
          save( bdd.clim, file=fn.bdd.climatology, compress=T )
          bdd = bdd.clim = NULL

          # save sp-time matrix for each stat .. easier to load into stmv this way
          for ( st in 1:length(p$bstats) ){
            BS = O[,,st]
            colnames(BS) = p$yrs
            fn = file.path( tstatdir_p1, paste("bottom.statistics.annual", p1$spatial_domain, p$bstats[st], "rdata", sep=".") )
            save( BS, file=fn, compress=TRUE )
            BS = NULL
            gc()
          }
          # climatology
          clim = matrix( NA, nrow=nrow(L1), ncol=length(p$bstats) )
          for (si in 1:length(p$bstats)) {
            clim[,si] = rowMeans(O[,,si], na.rm=T)
          }
          fn.climatology = file.path( tstatdir_p1, paste("bottom.statistics.climatology", p1$spatial_domain, "rdata", sep=".") )
          colnames(clim) = p$bstats
          save( clim, file=fn.climatology, compress=T )
          clim = NULL
          gc()
        }

    return ("Completed")
  }


  # -----------------


  if (DS %in% c("spatial.annual.seasonal", "spatial.annual.seasonal.redo") ) {
    #\\ spatial, temporal (annual and seasonal) .. means only
    #\\ copy in array format for domain/resolution of interest for faster lookups


    if ( DS=="spatial.annual.seasonal" ) {
      outdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p$spatial_domain )

      outfile =  file.path( outdir, paste( "temperature.spatial.annual.seasonal", ret, ".rdata", sep="") )
      O = NULL
      if (file.exists(outfile)) load( outfile )
      return (O)
    }

    grids = unique( c( p$spatial_domain_subareas, p$spatial_domain) )
    for (gr in grids ) {
      #  print(gr)
      p1 = bathymetry_parameters( spatial_domain=gr, project_class="default"  )  # default is the "best" performing method
      nlocs = nrow( bathymetry_db(p=p1, DS="baseline"))
      for (ret in c("mean", "lb", "ub") ) {
        O = array( NA, dim=c(nlocs, p$ny, p$nw ), dimnames=list(NULL, p$yrs, p$dyears) )
        for ( y in 1:p$ny ) {
          O[,y,] = temperature_db( p=p1, DS="predictions", yr=p$yrs[y], ret=ret )
        }
        outdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p1$spatial_domain )
        dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
        outfile =  file.path( outdir, paste("temperature.spatial.annual.seasonal", ret, ".rdata", sep="") )
        save (O, file=outfile, compress=T )
        print(outfile)
      }
    }
    return( outfile )

  }


  # -----------------


  if (DS %in% c( "timeslice", "timeslice.redo" )){

    tslicedir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p$spatial_domain )
    dir.create( tslicedir, showWarnings=F, recursive = TRUE )

    # priority to dyear_index if provided
    if (is.null(dyear_index)) {
      if (exists("dyears", p)) {
        if (exists("prediction_dyear", p)) {
          dyear_index = which.min( abs( p$prediction_dyear - p$dyears))
        }
      }
    }
    if (is.null(dyear_index)) dyear_index=1

    if (DS %in% c("timeslice")) {
      O = NULL
      outfile =  file.path( tslicedir, paste("bottom.timeslice", dyear_index, ret, "rdata", sep=".") )
      if (file.exists( outfile ) ) load(outfile)
      return ( O )
    }

    p0 = p  # the originating parameters
    grids = unique( c( p$spatial_domain_subareas , p$spatial_domain) )

    for (gr in grids ) {
      # print(gr)
      p1 = bathymetry_parameters( spatial_domain=gr, project_class="default"  )  # default is the "best" performing method
      tslicedir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p1$spatial_domain )
      nlocs = nrow( bathymetry_db(p=p1, DS="baseline"))
      dir.create(tslicedir, recursive=TRUE, showWarnings=FALSE)

      O = matrix(NA, ncol=p$ny, nrow=nlocs)
      colnames(O) = p$yrs

      for ( r in 1:p$ny ) {
        P = temperature_db( p=p1, DS="predictions", yr=p$yrs[r], ret="mean"  )
        if (!is.null(P))  O[,r] = P[,dyear_index]
        P = NULL
      }
      outfileP =  file.path( tslicedir, paste("bottom.timeslice", dyear_index, "mean", "rdata", sep=".") )
      save( O, file=outfileP, compress=T )
      print(outfileP)

      O = O * NA
      for ( r in 1:p$ny ) {
        Pl = temperature_db( p=p1, DS="predictions", yr=p$yrs[r], ret="lb"  )
        if (!is.null(Pl)) O[,r] = Pl[,dyear_index]
        Pl = NULL
      }
      outfileV =  file.path( tslicedir, paste("bottom.timeslice", dyear_index, "lb", "rdata", sep=".") )
      save( O, file=outfileV, compress=T )

      O = O * NA
      for ( r in 1:p$ny ) {
        Pu = temperature_db( p=p1, DS="predictions", yr=p$yrs[r], ret="ub"  )
        if (!is.null(Pu)) O[,r] = Pu[,dyear_index]
        Pu = NULL
      }
      outfileW =  file.path( tslicedir, paste("bottom.timeslice", dyear_index, "ub", "rdata", sep=".") )
      save( O, file=outfileW, compress=T )

    }

    return ("Completed")
  }

  # ----------------------------


  if (DS %in% c(  "stmv.stats", "stmv.stats.redo" )){


    outdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p$spatial_domain )


    if (DS %in% c("stmv.stats")) {
      stats = NULL
      fn = file.path( outdir, paste( "stmv.statistics", "rdata", sep=".") )
      if (file.exists(fn) ) load(fn)
      return( stats )
    }

    # downscale and warp from p(0) -> p1
    # default domain
    S0 = stmv_db( p=p, DS="stmv.stats" )
    Snames = colnames(S0)
    p0 = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="default"  )  # default is the "best" performing method
    L0 = bathymetry_db( p=p0, DS="baseline" )
    L0i = array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
    sreg = setdiff( p$spatial_domain_subareas, p$spatial_domain )

    for ( gr in sreg ) {
      p1 = bathymetry_parameters( spatial_domain=gr, project_class="default"  )  # default is the "best" performing method
      L1 = bathymetry_db( p=p1, DS="baseline" )
      L1i = array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
      L1 = planar2lonlat( L1, proj.type=p1$aegis_proj4string_planar_km )
      L1$plon_1 = L1$plon # store original coords
      L1$plat_1 = L1$plat
      L1 = lonlat2planar( L1, proj.type=p0$aegis_proj4string_planar_km )
      p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres,
        theta=p1$pres/3, xwidth=4*p1$pres, ywidth=4*p1$pres )
      # theta=p1$pres/3 assume at pres most of variance is accounted ... correct if dense pre-intepolated matrices .. if not can be noisy

      stats = matrix( NA, ncol=ncol(S0), nrow=nrow(L1) )
      for ( i in 1:ncol(S0) ) {
        stats[,i] = spatial_warp( S0[,i], L0, L1, p0, p1, "fast", L0i, L1i )
      }
      colnames(stats) = Snames
      outdir_p1 = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p1$spatial_domain )
      dir.create( outdir_p1, recursive=T, showWarnings=F )
      fn1_sg = file.path( outdir_p1, paste("stmv.statistics", "rdata", sep=".") )
      save( stats, file=fn1_sg, compress=T )
      print (fn1_sg)
    }
    return ("Completed")
  }



  # ----------------------------



  if (DS %in% c("complete", "complete.redo" )) {
    # static summaries

    if (DS=="complete") {
      TM = NULL
      outdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p$spatial_domain )
      outfile =  file.path( outdir, paste( "temperature", "complete", p$spatial_domain, "rdata", sep= ".") )
      if ( file.exists( outfile ) ) load( outfile )
      Tnames = names(TM)
      if (is.null(varnames)) varnames=Tnames
      varnames = intersect( Tnames, varnames )
      if (length(varnames) == 0) varnames=Tnames  # no match .. send all
      TM = TM[ , varnames]
      return(TM)
    }

    grids = unique( c(p$spatial_domain_subareas , p$spatial_domain ) ) # operate upon every domain

    for (gr in grids ) {
      # print(gr)

      p1 = bathymetry_parameters( spatial_domain=gr, project_class="default"  )  # default is the "best" performing method
      L1 = bathymetry_db(p=p1, DS="baseline")

      BS = temperature_db( p=p1, DS="stmv.stats" )
      colnames(BS) = paste("t", colnames(BS), sep=".")
      TM = cbind( L1, BS )

      CL = temperature_db( p=p1, DS="bottom.statistics.climatology" )
      colnames(CL) = paste(p$bstats, "climatology", sep=".")
      TM = cbind( TM, CL )

      # bring in last stats
      outdir = file.path( p$modeldir, p$stmv_model_label, p$project_class, paste(  p$stmv_global_modelengine, p$stmv_local_modelengine, sep="_"), voi, p1$spatial_domain )
      dir.create( outdir, recursive=T, showWarnings=F )
      outfile =  file.path( outdir, paste( "temperature", "complete", p1$spatial_domain, "rdata", sep= ".") )
      save( TM, file=outfile, compress=T )

      print( outfile )

    }

    return( outdir )
  }


  
}
