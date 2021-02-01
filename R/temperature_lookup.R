temperature_lookup = function( LOCS=NULL, TIMESTAMP=NULL, spatial_domain=NULL, lookup_from="core", lookup_to="points", FUNC=mean,  vnames="t", lookup_from_class="aggregated_data", tz="America/Halifax" ) {
  # lookup from rawdata

  # NOTE:: lookup_from_class = "aggregated_data" or "lonlat.highres"

  # z = temperature_lookup( LOCS=M[, c("lon", "lat")], spatial_domain=p$spatial_domain, lookup_from="core", lookup_to="points" , lookup_from_class="aggregated_data" ) # core=="rawdata"

  if (is.null(spatial_domain))  {
    pT = temperature_parameters(  project_class=lookup_from  )
  } else {
    pT = temperature_parameters( spatial_domain=spatial_domain, project_class=lookup_from  )
  }


  vnmod = pT$variabletomodel
  crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))


  if ( lookup_from %in% c("core") & lookup_to == "points" )  {
    # matching to point to point 
    # if any still missing then use stmv depths
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    
    if (! "POSIXct" %in% class(TIMESTAMP)  ) TIMESTAMP = as.POSIXct( TIMESTAMP, tz=tz, origin=lubridate::origin  )
    DAT = data.frame( yr = lubridate::year(TIMESTAMP) )
    DAT$dyear = lubridate::decimal_date( TIMESTAMP ) - DAT$yr

    LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS=lookup_from_class )  # raw data
    names(LU)[ which(names(LU) =="temperature.mean") ] = vnmod
    LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)

    LUT_map = array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) )
    LUS_map = array_map( "xy->1", LU[,c("plon","plat")], gridparams=pT$gridparams )

    T_map = array_map( "ts->1", DAT[,  c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) )
    M_map = array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=pT$gridparams )

    iLM = match( paste(M_map, T_map, sep="_"), paste(LUS_map, LUT_map, sep="_") )
    DAT[ , vnmod ] = LU[ iLM, paste(vnmod, "mean", sep="." ) ]

    return( DAT[ , vnmod ] )
  }


  if ( lookup_from %in% c("core") & lookup_to == "areal_units" )  {
    # point -> areal unit
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    
    if (! "POSIXct" %in% class(TIMESTAMP)  ) TIMESTAMP = as.POSIXct( TIMESTAMP, tz=tz, origin=lubridate::origin  )
    DAT = data.frame( yr = lubridate::year(TIMESTAMP) )
    DAT$dyear = lubridate::decimal_date( TIMESTAMP ) - DAT$yr

    LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS=lookup_from_class )  # raw data
    names(LU)[ which(names(LU) =="temperature.mean") ] = vnmod
    LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)
    
    LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
    st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
    LU = sf::st_transform( LU, crs=st_crs(LOCS) )
    vn2 = "z.mean"
    LOCS[, vnames] = aggregate( LU[, vn2], LOCS, FUNC, na.rm=TRUE ) [[vn2]] [iAS]
    return( LOCS[,vnames] )
  }


  if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "points" )  {
    # matching to point to point 
    # if any still missing then use stmv depths
    LU = temperature_db ( pT, DS="complete", varnames="all" )  # raw data
    LU = planar2lonlat(LU, proj.type=pT$aegis_proj4string_planar_km)
    
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    LOCS[,vnames] = LU[ match(
        array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=pT$gridparams ),
        array_map( "xy->1", LU[,c("plon","plat")], gridparams=pT$gridparams )
    ), vnames ]
    return( LOCS[,vnames] )
  }

  if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "areal_units" )  {
    # point -> areal unit
    LU = temperature_db ( pT, DS="complete", varnames="all" )  # raw data
    LU = planar2lonlat(LU, pT$aegis_proj4string_planar_km)
    
    LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
    st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
    LU = sf::st_transform( LU, crs=st_crs(LOCS) )
    for (vn in vnames) {
      LOCS[, vn] = aggregate( LU[, vn], LOCS, FUNC, na.rm=TRUE ) [[vn]] [iAS]
    }
    return( LOCS[,vnames] )
  }



  if ( lookup_from %in% c("carstm" ) & lookup_to == "points" )  {
    # point to areal unit
    LU = carstm_model( p=pT, DS="carstm_modelled_summary" ) 
    if (is.null(LU)) stop("Carstm predicted fields not found")

    AU = areal_units( p=pT )  #  poly associated with LU
    bm = match( AU$AUID, LU$AUID )
    AU[[vnames]] = LU[,vnames][ bm ]
    LU = NULL
    # now rasterize and re-estimate

    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    raster_template = raster( LOCS, res=pT$areal_units_resolution_km, crs=st_crs( LOCS ) ) # +1 to increase the area
    for (vn in vnames) {
      LL = fasterize::fasterize( AU, raster_template, field=vn )
      LOCS[[vn]] = sp::over( LOCS, LL[, vn ], fn=FUNC, na.rm=TRUE )
    }
    return( LOCS[,vnames] )
  }


  if ( lookup_from %in% c("carstm") & lookup_to == "areal_units" )  {
    # areal unit to areal unit
    LU = carstm_model( p=pT, DS="carstm_modelled_summary" ) 
    if (is.null(LU)) stop("Carstm predicted fields not found")

    AU = areal_units( p=pT )  #  poly associated with LU
    bm = match( AU$AUID, LU$AUID )
    AU[[vnames]] = LU[,vnames][ bm ]
    LU = NULL

    # now rasterize and re-estimate
    raster_template = raster( LOCS, res=pT$areal_units_resolution_km, crs=st_crs( LOCS ) ) # +1 to increase the area
    for (vn in vnames) {
      LL = fasterize::fasterize( AU, raster_template, field=vn )
      LOCS[[vn]] = sp::over( LOCS, LL[, vn ], fn=FUNC, na.rm=TRUE )
    }
    return( LOCS[,vnames] )
  } 

}
