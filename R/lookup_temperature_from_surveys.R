
lookup_temperature_from_surveys = function( p, locs, timestamp, vnames="t.mean"  ) {
  # only for stmv-based lookups
    domain = temperature.db(p=p, DS="aggregated_data" )

    locs = lonlat2planar( locs, proj.type=p$aegis_proj4string_planar_km )
    locs$plon = round(locs$plon / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km
    locs$plat = round(locs$plat / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km

    dyr_cuts = c(p$dyears, p$dyears[length(p$dyears)]+ diff(p$dyears)[1] )

    domain_map = paste(
      domain$plon,
      domain$plat,
      domain$yr,
      as.numeric( cut( domain$dyear, breaks=dyr_cuts , include.lowest=T, ordered_result=TRUE ) ),
      sep="." )

    if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
    yrs = lubridate::year(timestamp)
    locs_map = paste(
      locs$plon,
      locs$plat,
      yrs,
      as.numeric( cut( (lubridate::decimal_date( timestamp ) - yrs), breaks=dyr_cuts, include.lowest=T, ordered_result=TRUE ) ),
      sep ="."
    )
    locs_index = match( locs_map, domain_map )
    out = domain[locs_index, vnames]
  return(out)
}


