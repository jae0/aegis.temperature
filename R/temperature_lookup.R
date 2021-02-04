temperature_lookup = function( LOCS=NULL, LOCS_AU=NULL, AU=NULL, spatial_domain=NULL, lookup_from="core", lookup_to="points", FUNC=mean,  vnames="t", vnames_from=paste(vnames, "mean", sep="."), lookup_from_class="aggregated_data", tz="America/Halifax" ) {
 
  # z = temperature_lookup( LOCS=M[, c("lon", "lat")], spatial_domain=p$spatial_domain, lookup_from="core", lookup_to="points" , lookup_from_class="aggregated_data" ) # core=="rawdata"
message("need to check::  [match( APS$AUID, as.character( sppoly$AUID ) )] ")

  if (is.null(spatial_domain))  {
    pT = temperature_parameters(  project_class=lookup_from  )
  } else {
    pT = temperature_parameters( spatial_domain=spatial_domain, project_class=lookup_from  )
  }

  crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))


  if ( lookup_from %in% c("core") & lookup_to == "points" )  {
    # matching to point (LU ) to point (LOCS)
    # if any still missing then use stmv depths
    
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    
    if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp = as.POSIXct( LOCS$timestamp, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year( LOCS$timestamp ) 
    LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

    LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS=lookup_from_class )  # raw data
    LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)
    names(LU)[ which(names(LU) == vnames_from ) ] =  vnames

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
    # point (LU) -> areal unit (LOCS: AU/timestamp)
    #   $ t.predicted             : num [1:190, 1:20, 1:10] 2.37 2.01 5.27 1.98 1.56 ...
    # ..- attr(*, "dimnames")=List of 3
    # .. ..$ AUID : chr [1:190] "106" "107" "108" "121" ...
    # .. ..$ year : chr [1:20] "1999" "2000" "2001" "2002" ...
    # .. ..$ dyear: chr [1:10] "0.05" "0.15" "0.25" "0.35" ...

    if (!exists("AU")) AU = areal_units( p=pT )
    if (!exists("AUID", AU)) AU$AUID = as.character(1:nrow(AU))
    if (!exists("AUID", LOCS) | !exists("timestamp", LOCS) ) stop("require AUID and timestamp in LOCS") 

    if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp = as.POSIXct( LOCS$timestamp, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(LOCS$timestamp) 
    LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr


    LU = temperature_db ( p=pT, year.assessment=max(pT$yrs), DS=lookup_from_class )  # raw data
    LU = lonlat2planar(LU, proj.type=pT$aegis_proj4string_planar_km)
    names(LU)[ which(names(LU) ==vnames_from) ] =  vnames
    LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
    st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
    LU = sf::st_transform( LU, crs=st_crs(AU) )
    LU_map = paste( 
      st_points_in_polygons( pts=LU, polys=AU[, "AUID"], varname= "AUID" ), 
      array_map( "ts->1", st_drop_geometry( LU) [,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )
    
    LOCS = st_as_sf( LOCS, coords=c("lon","lat") )
    st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
    LOCS = sf::st_transform( LOCS, crs=st_crs(AU) )
    LOCS_map =  paste( 
      st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" ),  
      array_map( "ts->1", st_drop_geometry(LOCS)[ , c("yr", "dyear") ], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )

    LU_summ = tapply( st_drop_geometry(LU)[, vn], LU_map, FUN=FUNC, na.rm=TRUE )

    LOCS[ , vn ] = LU_summ[ match( LOCS_map, as.character( names( LU_summ )) ) ]

    return( LOCS[, vn] )
  }


  if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "points" )  {

    # matching to point (LU) to points (LOCS)
    LU = temperature_db ( p=pT, DS="spatial.annual.seasonal" )  # raw data
    LU = planar2lonlat(LU, proj.type=pT$aegis_proj4string_planar_km)
    LU_map = array_map( "xy->1", LU[, c("plon","plat")], gridparams=p$gridparams )
    
    LOCS = lonlat2planar(LOCS, proj.type=pT$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
    LOCS_map = array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams )
    LOCS_index = match( LOCS_map, LU_map )

    if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp = as.POSIXct( LOCS$timestamp, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(LOCS$timestamp) 
    LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

    TIMESTAMP_index = array_map( "ts->2", LOCS[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )

    return( LOCS[ cbind( LOCS_index, TIMESTAMP_index ) ] )
  }


   if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "areal_units" )  {
     # points (LU) -> areal units (LOCS)
     LU = temperature_db ( pT, DS="complete", varnames="all" )  # raw data
     LU = planar2lonlat(LU, pT$aegis_proj4string_planar_km)
    
     LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
     st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
     LU = sf::st_transform( LU, crs=st_crs(LOCS) )

     if (!exists("AUID", LOCS_AU)) LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))

    if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp = as.POSIXct( LOCS$timestamp, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(LOCS$timestamp) 
    LOCS$dyear = lubridate::decimal_date( LOCS$TIMESTAMP ) - LOCS$yr
    TIMESTAMP_index = array_map( "ts->2", LOCS [, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
  
 
    # now rasterize and re-estimate
    LOCS$AU_index = match( LOCS$AUID, LU$AUID  )    # assuming AUID's are consistent
    
    # LOCS_AU .... must be sent ... <---------
    LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(pT$aegis_proj4string_planar_km) )


  #   LOCS_AUID = st_points_in_polygons( pts=st_as_sf( LOCS, coords=c("lon","lat"), crs=crs_lonlat ), polys = LU[, "AUID"], varname= "AUID" )
    
  #   LOCS_map =  paste( 
  #     as.character( LOCS_AUID ),  
  #     array_map( "ts->1", LOCS[ , c("yr", "dyear") ], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
  #     sep="_"
  #   )

     LU_map = paste( 
       st_points_in_polygons( pts=LU, polys=LOCS_AU[, "AUID"], varname= "AUID" ),
       array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
       sep="_"
     )
    

  #   for (vn in vnames) {
  #     LU_summ = tapply( LU[, vn], LU_uid, FUN=FUNC, na.rm=TRUE )
  #     LOCS[ , vn ] = LU_summ[ match( LOCS_map, as.character( names( LU_summ )) ) ]
  #   }
  #   return( st_drop_geometry(LOCS)[,vnames] )
   }



  if ( lookup_from %in% c("carstm" ) & lookup_to == "points" )  {
     # areal unit (LU) to points (LOCS)
    LU = carstm_model( p=pT, DS="carstm_modelled_summary" ) 
    if (is.null(LU)) stop("Carstm predicted fields not found")
    if (!exists(vnames_from, LU)) {
      message( "vnames_from: ", vnames_from, "not found. You probably want: t.predicted" )
      stop()
    }

    if (is.null(AU)) AU = areal_units( p=pT )
    if (!exists("AUID", AU)) AU$AUID = as.character(1:nrow(AU))
    AU = sf::st_transform( AU, crs=st_crs(pT$aegis_proj4string_planar_km) )
    
    bm = match( AU$AUID, LU$AUID )
      
    LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
    st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
    LOCS = sf::st_transform( LOCS, crs=st_crs(pT$aegis_proj4string_planar_km) )
    LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" )   
    LOCS$AU_index = match( LOCS$AUID, LU$AUID  )    
    
    if (! "POSIXct" %in% class(LOCS$TIMESTAMP)  ) LOCS$TIMESTAMP = as.POSIXct( LOCS$TIMESTAMP, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(LOCS$TIMESTAMP) 
    LOCS$dyear = lubridate::decimal_date( LOCS$TIMESTAMP ) - LOCS$yr
    TIMESTAMP_index = array_map( "ts->2", st_drop_geometry(LOCS) [, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
  
    # need to check bm <<<---
    LOCS[,vnames] = LU[[vnames_from]][ cbind( LOCS$AU_index[bm], TIMESTAMP_index )]

    return( LOCS[,vnames] )  
  
  }


  if ( lookup_from %in% c("carstm") & lookup_to == "areal_units" )  {
    # areal unit (LU) to areal units (AU/LOCS) 

    LU = carstm_model( p=pT, DS="carstm_modelled_summary" ) 
    if (is.null(LU)) stop("Carstm predicted fields not found")
    if (!exists(vnames_from, LU)) {
      message( "vnames_from: ", vnames_from, "not found. You probably want: t.predicted" )
      stop()
    }

    if (is.null(AU)) AU = areal_units( p=pT )
    if (!exists("AUID", AU)) AU$AUID = as.character(1:nrow(AU))
    AU = sf::st_transform( AU, crs=st_crs(pT$aegis_proj4string_planar_km) )
    
    bm = match( AU$AUID, LU$AUID ); message("must check bm")


    if (! "POSIXct" %in% class(LOCS$TIMESTAMP)  ) LOCS$TIMESTAMP = as.POSIXct( LOCS$TIMESTAMP, tz=tz, origin=lubridate::origin  )
    LOCS$yr = lubridate::year(LOCS$TIMESTAMP) 
    LOCS$dyear = lubridate::decimal_date( LOCS$TIMESTAMP ) - LOCS$yr
    TIMESTAMP_index = array_map( "ts->2", LOCS [, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
  
    # now rasterize and re-estimate
    LOCS$AU_index = match( LOCS$AUID, LU$AUID  )    # assuming AUID's are consistent
    
    # LOCS_AU .... must be sent ... <---------
    LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(pT$aegis_proj4string_planar_km) )

    raster_template = raster( LOCS_AU, res=min(pT$gridparams$res), crs=st_crs( pT$aegis_proj4string_planar_km ) ) 
    
    LL = fasterize::fasterize( AU, raster_template, field=vnames[1] )
    LOCS_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
    st_crs(LOCS_pts) = st_crs( LOCS )

    # map LOCS to AU
    LOCS_map =  paste( 
      st_points_in_polygons( pts=LOCS_pts, polys=AU[, "AUID"], varname="AUID" ),  
      array_map( "ts->1", LOCS[ , c("yr", "dyear") ], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )

    LU_map = paste( 
      st_points_in_polygons( pts=LU, polys = AU[, "AUID"], varname= "AUID" ), 
      array_map( "ts->1", st_drop_geometry( LU) [,c("yr", "dyear")], dims=c(pT$ny, pT$nw), res=c( 1, 1/pT$nw ), origin=c( min(pT$yrs), 0) ), 
      sep="_"
    )
    
    for (nn in 1:length(vnames)) {
      vn = vnames[nn]
      if (nn > 1) {
        LL = fasterize::fasterize( LU, raster_template, field=vn )
        LOCS_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
        st_crs(LOCS_pts) = st_crs( LOCS )
      } 
      LU_summ = tapply( st_drop_geometry(LU)[, vn], LU_map, FUN=FUNC, na.rm=TRUE )
      LOCS[ , vn ] = LU_summ[ match( LOCS_map, as.character( names( LU_summ )) ) ]
    }

    return( LOCS[,vnames] )
 
  } 

  
}
