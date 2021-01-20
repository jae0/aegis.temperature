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
      M_dyear_discret = discretize_data( M$dyear[ii], pT$discretization$dyear )  # LU$dyear is discretized. . match discretization
      M$uid =  paste(M_AUID, M$yr[ii], M_dyear_discret, sep=".")

      LU$AUID = st_points_in_polygons(
        pts = st_as_sf( LU, coords=c("lon","lat"), crs=crs_lonlat ),
        polys = sppoly[, "AUID"],
        varname="AUID"
      )
      LU_dyear_discret = discretize_data( LU$dyear, pT$discretization$dyear )
      LU$uid = paste(LU$AUID, LU$yr, LU_dyear_discret, sep=".")

      LU = tapply( LU[, paste(vnmod, "mean", sep="." )], LU$uid, FUN=median, na.rm=TRUE )

      jj = match( as.character( M_AUID), as.character( names(LU )) )
      M[ ii, vnmod ] = LU[jj]
    }
  }

  if (lookup_mode %in% c("stmv",  "hybrid") ) {
      # if any still missing then use stmv depths
    pC = temperature_parameters( spatial_domain=pT$spatial_domain, project_class=lookup_mode  )
    ii = NULL
    ii =  which( !is.finite( M[ , vnmod ] ))
    if (length(ii) > 0) {

      L1 = bathymetry_db(p=pC, DS="baseline")

      BS = aegis_db( p=pC, DS="stmv.stats" )
      colnames(BS) = paste(pC$stmv_variables$Y, colnames(BS), sep=".")
      IC = cbind( L1, BS )

      # climatology
      nL1 = nrow(L1)
      PS = matrix( NA, nrow=nL1, ncol=pT$ny )

        for (iy in 1:pT$ny) {
          yr = pT$yrs[iy]
          oo=NULL;

          oo = temperature_db(p=pC, DS="predictions", ret="mean", yr=yr)

          if (!is.null(oo)) PS[,iy] = oo


      LU = temperature_db ( pC, DS="complete", varnames="all" )  # raw data
      LU = planar2lonlat(LU, proj.type=pT$aegis_proj4string_planar_km)
      LU = LU[ which( LU$lon > pT$corners$lon[1] & LU$lon < pT$corners$lon[2]  & LU$lat > pT$corners$lat[1] & LU$lat < pT$corners$lat[2] ), ]

## --- add time
  # only temp for now
      yrs = lubridate::year(timestamp)
      dyear = lubridate::decimal_date( timestamp ) - yrs
      dyear_breaks = c(pT$dyears, pT$dyears[length(pT$dyears)]+ diff(pT$dyears)[1] )
      dyear_index = as.numeric( cut( dyear, breaks=dyear_breaks, include.lowest=TRUE, ordered_result=TRUE, right=FALSE ) )
      DB_years = as.numeric( dimnames(DB)[[2]] )
      dindex = cbind(locsmap, match( yrs, DB_years ), dyear_index )

      outnames = NULL
      for (vn in varnames) {
        if (vn=="t") ret = "mean"
        if (vn=="tlb") ret = "lb"
        if (vn=="tub") ret = "ub"
        if (vn %in% c("t", "tlb", "tub"))  pT$stmv_variables$Y = "t"  # required to force use of directory "t"
        DB = NULL
        DB=aegis_db(p=p, DS="spatial.annual.seasonal", ret=ret) # at this point this is the only database with seasonality .. other stats (than mean) will require supplemntary functionss
        if (!is.null(DB)) {
          out = cbind( out, DB[dindex] )
          outnames = c(outnames, vn)
        }
      }
      colnames(out) = outnames



      M[ ii, vnmod ] = LU[ match(
        array_map( "xy->1", M[ii, c("plon","plat")], gridparams=pT$gridparams ),
        array_map( "xy->1", LU[,c("plon","plat")], gridparams=pT$gridparams )
      ), vnmod ]


    }
  }


  return( M[ , vnmod ] )

}
