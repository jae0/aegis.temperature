
lookup_temperature_from_surveys = function( p, locs, timestamp, vnames="t.mean"  ) {

    pST = spatial_parameters( spatial_domain=p$spatial_domain )
    if (!exists("inputdata_spatial_discretization_planar_km", pST)) {
      if (!exists("inputdata_spatial_discretization_planar_km", p)) {
        pST$inputdata_spatial_discretization_planar_km = p$inputdata_spatial_discretization_planar_km
      } else {
        pST$inputdata_spatial_discretization_planar_km = 1
      }
    }

    if (!exists("inputdata_temporal_discretization_yr", pST)) {
      if (!exists("inputdata_temporal_discretization_yr", p)) {
        pST$inputdata_temporal_discretization_yr = p$inputdata_temporal_discretization_yr
      } else {
        pST$inputdata_temporal_discretization_yr = 1 / 52
      }
    }
    if (!exists("variabletomodel", pST)) pST$variabletomodel = "t"

    if (!exists( "yrs", pST)) pST$yrs = 1910:lubridate::year(lubridate::now())  # default
    nw = 1 / pST$inputdata_temporal_discretization_yr
    dyears_cuts = (c(1:nw)-1) / nw # intervals of decimal years... fractional year breaks
    dyears_cuts = c(dyears_cuts, dyears_cuts[length(dyears_cuts)]+ diff(dyears_cuts)[1] )

    domain = temperature.db(p=pST, DS="aggregated_data" )
    domain = lonlat2planar( domain, proj.type=pST$aegis_proj4string_planar_km )
    domain$plon = round(domain$plon / pST$inputdata_spatial_discretization_planar_km + 1 ) * pST$inputdata_spatial_discretization_planar_km
    domain$plat = round(domain$plat / pST$inputdata_spatial_discretization_planar_km + 1 ) * pST$inputdata_spatial_discretization_planar_km

    locs = lonlat2planar( locs, proj.type=pST$aegis_proj4string_planar_km )
    locs$plon = round(locs$plon / pST$inputdata_spatial_discretization_planar_km + 1 ) * pST$inputdata_spatial_discretization_planar_km
    locs$plat = round(locs$plat / pST$inputdata_spatial_discretization_planar_km + 1 ) * pST$inputdata_spatial_discretization_planar_km

    domain_map = paste(
      domain$plon,
      domain$plat,
      domain$yr,
      as.numeric( cut( domain$dyear, breaks=dyears_cuts , include.lowest=T, ordered_result=TRUE ) ),
      sep="." )

    if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
    yrs = lubridate::year(timestamp)
    locs_map = paste(
      locs$plon,
      locs$plat,
      yrs,
      as.numeric( cut( (lubridate::decimal_date( timestamp ) - yrs), breaks=dyears_cuts, include.lowest=T, ordered_result=TRUE ) ),
      sep ="."
    )
    locs_index = match( locs_map, domain_map )
    out = domain[locs_index, vnames]
  return(out)
}


