temperature_lookup_rawdata = function( M, spatial_domain=NULL, sppoly=NULL,   tz="America/Halifax", lookup_mode="stmv" ) {
  # lookup from rawdata

  if (is.null(spatial_domain))  {
    pT = temperature_parameters(  project_class="core"  )
  } else {
    pT = temperature_parameters( spatial_domain=spatial_domain, project_class="core"  )
  }

  vnmod = pT$variabletomodel
  crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))

  M = as.data.frame( M )
  names(M) = c("lon", "lat", "timestamp")
  M = lonlat2planar(M, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
  if (! "POSIXct" %in% class(M$timestamp)  ) M$timestamp = as.POSIXct( M$timestamp, tz=tz, origin=lubridate::origin  )
  M$yr = lubridate::year(M$timestamp)
  M$dyear = lubridate::decimal_date( M$timestamp ) - M$yr

  LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS="aggregated_data" )  # raw data
  names(LU)[ which(names(LU) =="temperature.mean") ] = vnmod
  LU = LU[ which( LU$lon > pT$corners$lon[1] & LU$lon < pT$corners$lon[2]  & LU$lat > pT$corners$lat[1] & LU$lat < pT$corners$lat[2] ), ]
  LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)

  LUT_map = array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) )
  LUS_map = array_map( "xy->1", LU[,c("plon","plat")], gridparams=pT$gridparams )

  T_map = array_map( "ts->1", M[, c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) )
  M_map = array_map( "xy->1", M[, c("plon","plat")], gridparams=pT$gridparams )

  iLM = match( paste(M_map, T_map, sep="_"), paste(LUS_map, LUT_map, sep="_") )
  M[ , vnmod ] = LU[ iLM, paste(vnmod, "mean", sep="." ) ]

  gc()

  if (!is.null(sppoly)) {
        # if any still missing then use a mean by AUID
    ii = NULL
    ii =  which( !is.finite(M[ , vnmod ]))
    if (length(ii) > 0) {
      if (!exists("AUID", M)) {
        M_AUID = st_points_in_polygons(
          pts = st_as_sf( M[ii,], coords=c("lon","lat"), crs=crs_lonlat ),
          polys = sppoly[, "AUID"],
          varname = "AUID"
        )
        M_AUID = as.character( M_AUID )  # match each datum to an area
      }
      T_map = array_map( "ts->1", M[ii, c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) )
      M_uid =  paste(M_AUID,  T_map, sep=".")

      LU$AUID = st_points_in_polygons(
        pts = st_as_sf( LU, coords=c("lon","lat"), crs=crs_lonlat ),
        polys = sppoly[, "AUID"],
        varname="AUID"
      )
      
      LUT_map = array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) )
      LU$uid = paste(LU$AUID, LUT_map, sep=".")

      LU = tapply( LU[, paste(vnmod, "mean", sep="." )], LU$uid, FUN=median, na.rm=TRUE )

      jj = match( as.character( M_uid), as.character( names(LU )) )
      M[ ii, vnmod ] = LU[jj]
    }
  }


  return( M[ , vnmod ] )

}
