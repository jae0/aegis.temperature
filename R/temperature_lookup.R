
temperature_lookup = function( p, locs, timestamp, vnames="t", output_data_class="points", source_data_class="aggregated_rawdata", locs_proj4string=NULL, tz="America/Halifax" ) {

  # if locs is points, then need to send info on projection as an attribute proj4string"

  ## TODO:: a generic one where sp/sf info is embedded in locs and appropriate spatial.domain is chosen for lookup

  require(aegis.temperature)

  if (p$project_name != "temperature")  {
    p = temperature_parameters(p=parameters_reset(p), project_name="temperature", year.assessment=max(p$yrs) )
    warning( "Parameter list may be inconsistent")
  }

  # load input data or reformat it
   if (source_data_class=="rawdata") {

      B = temperature_db ( p=p, DS="lonlat.highres" )  # 16 GB in RAM just to store!
#      Bnames = c("lon", "lat", "grainsize", "plon", "plat"),

   } else if (source_data_class=="aggregated_rawdata") {

      B = temperature_db ( p=p, DS="aggregated_data" )+
#       Bnames = c("t.mean", "t.sd",  "t.n", "id",  "plon", "plat", "yr", "dyear", "lon", "lat")
      B$t = B$t.mean
      B$t.mean  = NULL

   } else if (source_data_class=="stmv") {

      B = temperature_db(p=p, DS="spatial.annual.seasonal"  )
    # Bnames = c( "plon", "plat", "t", "t.lb", "t.ub",
    #   "t.sdTotal", "t.rsquared", "t.ndata", "t.sdSpatial", "t.sdObs", "t.phi", "t.nu", ts.localrange" )
      zname = "t"

   } else if (source_data_class=="carstm") {

      Bcarstm = carstm_summary( p=p ) # to load currently saved sppoly
      B = areal_units( p=p )
      bm = match( B$AUID, Bcarstm$AUID )
      B$t  = Bcarstm$t.predicted[ bm,, ]
      B$t.se = Bcarstm$t.predicted_se[ bm,,  ]
      Bcarstm = NULL
      zname = "t"
  }

  Bnames = setdiff( names(B), c("AUID", "uid", "layer", "plon", "plat", "lon", "lat", "au_sa_km2",
    "cfanorth_surfacearea", "cfasouth_surfacearea", "cfa23_surfacearea",  "cfa24_surfacearea", "cfa4x_surfacearea" ) )


  if (output_data_class == "points ") {

    if ( source_data_class %in% c("rawdata", "aggregated_rawdata", "stmv" ) )  {

      if ( is.null( locs_proj4string) ) locs_proj4string = attr( locs, "proj4string" )
      if ( is.null( locs_proj4string ) ) {
        # assume projection is the same as that specified by "aegis_proj4string_planar_km"
        locs_proj4string = p$aegis_proj4string_planar_km
        names( locs) = c("plon", "plat")
      }
      if ( locs_proj4string =="lonlat" ) {
        names( locs) = c("lon", "lat")
        locs = lonlat2planar( locs[, c("lon", "lat")], proj.type=p$aegis_proj4string_planar_km )
        locs_proj4string = p$aegis_proj4string_planar_km
      }
      if ( locs_proj4string != p$aegis_proj4string_planar_km ) {
        locs = planar2lonlat( locs[, c("plon", "plat")], proj.type=locs_proj4string )
        locs = lonlat2planar( locs[, c("lon", "lat")], proj.type=p$aegis_proj4string_planar_km )
        locs_proj4string = p$aegis_proj4string_planar_km
      }

      if (source_data_class=="stmv") {
        B_map = array_map( "xy->1", B[,c("plon","plat")], gridparams=p$gridparams )
        locs_map = array_map( "xy->1", locs[,c("plon","plat")], gridparams=p$gridparams )

        locs_index = match( locs_map, B_map )

        # if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="America/Halifax", origin=lubridate::origin  )

        # yrs = lubridate::year(timestamp)
        # yrs_index = match( yrs, p$yrs )
        # dyear = lubridate::decimal_date( timestamp ) - yrs
        # dyear_index = as.numeric( cut( dyear, breaks=c(p$dyears, p$dyears[length(p$dyears)]+ diff(p$dyears)[1] ) , include.lowest=T, ordered_result=TRUE ) )
        # dindex = cbind(locs_index, yrs_index, dyear_index ) # check this
        if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
        tstamp = data.frame( yr = lubridate::year(timestamp) )
        tstamp$dyear = lubridate::decimal_date( timestamp ) - tstamp$yr
        timestamp_map = array_map( "ts->2", tstamp[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )

        dindex = cbind(locs_index, timestamp_map ) # check this
        p$stmv_variables = NULL  # this can exist and cause confusion
        return( B[dindex])
      }

      if (source_data_class=="aggregated_rawdata") {
        T_map = array_map( "ts->1", B[,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
        B_map = array_map( "xy->1", B[,c("plon","plat")], gridparams=gridparams )
        if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
        tstamp = data.frame( yr = lubridate::year(timestamp) )
        tstamp$dyear = lubridate::decimal_date( timestamp ) - tstamp$yr
        timestamp_map = array_map( "ts->1", tstamp[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
        locs_map = array_map( "xy->1", locs[,c("plon","plat")], gridparams=gridparams )
        locs_index = match( paste(locs_map, timestamp_map, sep="_"), paste(B_map, T_map, sep="_") )
        out = B[locs_index, vnames]
      return(out)

      }

      vnames = intersect( names(B), vnames )
      if ( length(vnames) ==0 ) vnames=names(B) # no match returns all
      return( B[locs_index, vnames] )
    }

    if ( source_data_class=="carstm") {
      # convert to raster then match
      require(raster)
      raster_template = raster(extent(locs))
      res(raster_template) = p$areal_units_resolution_km  # crs usually in meters, but aegis's crs is in km
      crs(raster_template) = projection(locs) # transfer the coordinate system to the raster

      if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
      tstamp = data.frame( yr = lubridate::year(timestamp) )
      tstamp$dyear = lubridate::decimal_date( timestamp ) - tstamp$yr
      timestamp_map = array_map( "ts->2", tstamp[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
# TODO
stop("not finished ... must addd time lookup")  # TODO
# TODO

      dindex = cbind(locs_index, timestamp_map ) # check this

      locs = sf::st_as_sf( as.data.frame(locs), coords=c(1, 2) )
      st_crs(locs) = crs(B)
      for (vn in Bnames) {
        Bf = fasterize::fasterize( as(B, "sf"), raster_template, field=vn )
        vn2 = paste(vn, "sd", sep="." )
        locs[, vn ] = raster::extract( Bf, locs, fun=mean, na.rm=TRUE)
        locs[, vn2] = raster::extract( Bf, locs, fun=sd, na.rm=TRUE)
      }
      vnames = intersect( names(B), vnames )
      if ( length(vnames) ==0 ) vnames=names(B) # no match returns all
      return( as.matrix(locs[[vnames]]) )
    }
  }


  if ( output_data_class=="areal_units") {

    # expects loc to be a spatial polygon data frame

    if ( source_data_class %in% c("rawdata", "aggregated_rawdata", "stmv" ) ) {

      if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
      tstamp = data.frame( yr = lubridate::year(timestamp) )
      tstamp$dyear = lubridate::decimal_date( timestamp ) - tstamp$yr
      timestamp_map = array_map( "ts->2", tstamp[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
# TODO
stop("not finished ... must addd time lookup")  # TODO
# TODO

      dindex = cbind(locs_index, timestamp_map ) # check this

      Bsf = sf::st_as_sf( B, coords=c("lon", "lat") )
      st_crs(Bsf) = CRS( projection_proj4string("lonlat_wgs84") )
      Bsf = sf::st_transform( Bsf, crs=CRS(proj4string(locs)) )
      for (vn in Bnames) {
        vn2 = paste(vn, "sd", sep="." )
        #Bf= ...
        locs[,vn] = st_polygons_in_polygons( locs, Bf[,vn], fn=mean, na.rm=TRUE )
        locs[,vn2] = st_polygons_in_polygons( locs, Bf[,vn], fn=sd, na.rm=TRUE )
      }
      vnames = intersect( names(B), vnames )
      if ( length(vnames) ==0 ) vnames=names(B) # no match returns all
      return(locs[, vnames] )
    }


    if ( source_data_class=="carstm") {

      if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
      tstamp = data.frame( yr = lubridate::year(timestamp) )
      tstamp$dyear = lubridate::decimal_date( timestamp ) - tstamp$yr
      timestamp_map = array_map( "ts->2", tstamp[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
# TODO
stop("not finished ... must addd time lookup")  # TODO
# TODO

      dindex = cbind(locs_index, timestamp_map ) # check this

      # convert to raster then match
      require(raster)
      raster_template = raster(extent(locs)) # +1 to increase the area
      res(raster_template) = p$areal_units_resolution_km  # crs usually in meters, but aegis's crs is in km
      crs(raster_template) = projection(locs) # transfer the coordinate system to the raster
      Bsf = sf::st_transform( as(B, "sf"), crs=CRS(proj4string(locs)) )  # B is a carstm sppoly
      Boo = as(B, "SpatialPolygonsDataFrame")
      for (vn in Bnames) {
        # Bf = fasterize::fasterize( Bsf, raster_template, field=vn )
        vn2 = paste(vn, "sd", sep="." )
        locs[,vn] = st_polygons_in_polygons( locs, Bf[,vn], fn=mean, na.rm=TRUE )
        locs[,vn2] = st_polygons_in_polygons( locs, Bf[,vn], fn=sd, na.rm=TRUE )
      }
      vnames = intersect( names(B), vnames )
      if ( length(vnames) ==0 ) vnames=names(B) # no match returns all
      return(locs[,vnames])
   }
  }

}


