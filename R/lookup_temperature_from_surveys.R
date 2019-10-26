
lookup_temperature_from_surveys = function( p, locs, timestamp  ) {
  # only for stmv-based lookups
    domain = temperature.db(p=p, DS="aggregated_data" )
    dyr_cuts = c(p$dyears, p$dyears[length(p$dyears)]+ diff(p$dyears)[1] )

    domain_index = paste(
      stmv::array_map( "xy->1", domain[,c("plon", "plat")], gridparams=p$gridparams ),
      domain$yr,
      as.numeric( cut( domain$dyear, breaks=dyr_cuts , include.lowest=T, ordered_result=TRUE ) ),
      sep="." )

    if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="UTC", origin=lubridate::origin  )
    yrs = lubridate::year(timestamp)
    locs_index = paste(
      stmv::array_map( "xy->1", locs, gridparams=p$gridparams ),
      yrs,
      as.numeric( cut( (lubridate::decimal_date( timestamp ) - yrs), breaks=dyr_cuts, include.lowest=T, ordered_result=TRUE ) ),
      sep ="."
    )
    ii = match( locs_index, domain_index )
    out = domain$t.mean[ii]
  return(out)
}


