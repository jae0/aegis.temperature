temperature_lookup = function( LOCS=NULL, TIMESTAMP=NULL, spatial_domain=NULL, lookup_from="core", lookup_to="points", FUNC=mean,  vnames="t", lookup_from_class="aggregated_data", tz="America/Halifax" ) {
 
  # z = temperature_lookup( LOCS=M[, c("lon", "lat")], spatial_domain=p$spatial_domain, lookup_from="core", lookup_to="points" , lookup_from_class="aggregated_data" ) # core=="rawdata"

  if (is.null(spatial_domain))  {
    pT = temperature_parameters(  project_class=lookup_from  )
  } else {
    pT = temperature_parameters( spatial_domain=spatial_domain, project_class=lookup_from  )
  }

  crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))


  if ( lookup_from %in% c("core") & lookup_to == "points" )  {
    # matching to point (LU ) to point (LOCS)
    # if any still missing then use stmv depths
    vn = "t"
    vn2 = paste( vn, "mean", sep="." )
    
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    
    if (! "POSIXct" %in% class(TIMESTAMP)  ) TIMESTAMP = as.POSIXct( TIMESTAMP, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(TIMESTAMP) 
    LOCS$dyear = lubridate::decimal_date( TIMESTAMP ) - LOCS$yr

    LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS=lookup_from_class )  # raw data
    LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)
    names(LU)[ which(names(LU) == vn2 ) ] =  vn

    LU_map = paste( 
      array_map( "xy->1", LU[,c("plon","plat")], gridparams=pT$gridparams ), 
      array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_" 
    )

    LOCS_map = paste(
      array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=pT$gridparams ), 
      array_map( "ts->1", LOCS[, c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )

    LOCS[ , vn ] = LU[ match( LOCS_map, LU_map ), vn ]

    return( LOCS[ , vn ] )
  }


  if ( lookup_from %in% c("core") & lookup_to == "areal_units" )  {
    # point (LU) -> areal unit (LOCS)
    vn = "t"
    vn2 = paste( vn, "mean", sep="." )

    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    
    if (! "POSIXct" %in% class(TIMESTAMP)  ) TIMESTAMP = as.POSIXct( TIMESTAMP, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(TIMESTAMP) 
    LOCS$dyear = lubridate::decimal_date( TIMESTAMP ) - LOCS$yr

    LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS=lookup_from_class )  # raw data
    LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)
    names(LU)[ which(names(LU) ==vn2) ] =  vn
    
    LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
    st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
    
    LU = sf::st_transform( LU, crs=st_crs(LOCS) )

    if (!exists("AUID", LU)) LU$AUID = as.character(1:nrow(LU))
    LU_uid = paste( 
      LU$AUID, 
      array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )
   
    LOCS_AUID = st_points_in_polygons( pts=st_as_sf( LOCS, coords=c("lon","lat"), crs=crs_lonlat ), polys = LU[, "AUID"], varname= "AUID" )
    
    LOCS_map =  paste( 
      as.character( LOCS_AUID ),  
      array_map( "ts->1", LOCS[ , c("yr", "dyear") ], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )

    LU_summ = tapply( LU[, vn2], LU_uid, FUN=FUNC, na.rm=TRUE )
    LOCS[ , vn ] = LU_summ[ match( LOCS_map, as.character( names( LU_summ )) ) ]
    
    return( LOCS[, vn] )
  }


  if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "points" )  {
    # matching to point (LU) to points (LOCS)
    LU = temperature_db ( pT, DS="spatial.annual.seasonal" )  # raw data
    LU = planar2lonlat(LU, proj.type=pT$aegis_proj4string_planar_km)
    
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    
    LU_map = array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams )
    LOCS_map = array_map( "xy->1", LOCS[,c("plon","plat")], gridparams=p$gridparams )
    
    if (! "POSIXct" %in% class(TIMESTAMP)  ) TIMESTAMP = as.POSIXct( TIMESTAMP, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(TIMESTAMP) 
    LOCS$dyear = lubridate::decimal_date( TIMESTAMP ) - LOCS$yr

    LOCS_index = cbind( 
      match( LOCS_map, B_map ),
      array_map( "ts->2", LOCS[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
    )

    return( LOCS[ LOCS_index ] )
  }

  if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "areal_units" )  {
    # points (LU) -> areal units (LOCS)
    LU = temperature_db ( pT, DS="complete", varnames="all" )  # raw data
    LU = planar2lonlat(LU, pT$aegis_proj4string_planar_km)
    
    LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
    st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
    LU = sf::st_transform( LU, crs=st_crs(LOCS) )

    if (!exists("AUID", LU)) LU$AUID = as.character(1:nrow(LU))
    LU_uid = paste( 
      LU$AUID, 
      array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )
   
    LOCS_AUID = st_points_in_polygons( pts=st_as_sf( LOCS, coords=c("lon","lat"), crs=crs_lonlat ), polys = LU[, "AUID"], varname= "AUID" )
    
    LOCS_map =  paste( 
      as.character( LOCS_AUID ),  
      array_map( "ts->1", LOCS[ , c("yr", "dyear") ], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )

    for (vn in vnames) {
      LU_summ = tapply( LU[, vn], LU_uid, FUN=FUNC, na.rm=TRUE )
      LOCS[ , vn ] = LU_summ[ match( LOCS_map, as.character( names( LU_summ )) ) ]
    }
    return( st_drop_geometry(LOCS)[,vnames] )
  }



  if ( lookup_from %in% c("carstm" ) & lookup_to == "points" )  {
     # areal unit (LU) to points (LOCS)
    LU_summ = carstm_model( p=pB, DS="carstm_modelled_summary" ) 
    if (is.null(LU_summ)) stop("Carstm predicted fields not found")

    LU = areal_units( p=pB )  #  poly associated with LU
    LU = sf::st_transform( LU, crs=st_crs(pB$aegis_proj4string_planar_km) )
    bm = match( LU$AUID, LU_summ$AUID )
    LU[[vnames]] = LU_summ[,vnames][ bm ]
    LU_summ = NULL

    LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
    st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
    LOCS = sf::st_transform( LOCS, crs=st_crs(pB$aegis_proj4string_planar_km) )

    raster_template = raster( LOCS, res=min(pB$gridparams$res), crs=st_crs( LOCS ) ) 
    for (vn in vnames) {
      LL = fasterize::fasterize( LU, raster_template, field=vn )
      o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
      st_crs(o) = st_crs( LOCS )
    
      LOCS[,vn] = st_drop_geometry(o)[ match(
        array_map( "xy->1", st_coordinates(LOCS), gridparams=pB$gridparams ),
        array_map( "xy->1", st_coordinates(o), gridparams=pB$gridparams )
      ), "layer" ]
    }
    
    return( st_drop_geometry(LOCS)[,vnames] )
 }


  if ( lookup_from %in% c("carstm") & lookup_to == "areal_units" )  {
    # areal unit (LU) to areal units (LOCS) 
    LU_summ = carstm_model( p=pB, DS="carstm_modelled_summary" ) 
    if (is.null(LU_summ)) stop("Carstm predicted fields not found")

    LU = areal_units( p=pB )  #  poly associated with LU
    bm = match( LU$AUID, LU_summ$AUID )
    LU[[vnames]] = LU_summ[,vnames][ bm ]
    LU_summ = NULL

    # now rasterize and re-estimate
    raster_template = raster( LOCS, res=min(pB$gridparams$res), crs=st_crs( LOCS ) ) 
    for (vn in vnames) {
      LL = fasterize::fasterize( LU, raster_template, field=vn )
      o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
      st_crs(o) = st_crs( LOCS )
      LOCS[, vn] = sf:::aggregate.sf( o, LOCS, FUNC, na.rm=TRUE ) [["layer"]]  
    }

    return( st_drop_geometry(LOCS)[,vnames] )
  } 

}
