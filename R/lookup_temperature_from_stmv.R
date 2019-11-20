
lookup_temperature_from_stmv = function( p, locs, timestamp  ) {
  # only for stmv-based lookups
    domain = bathymetry.db(p=p, DS="baseline" )
    domain_map = stmv::array_map( "xy->1", domain, gridparams=p$gridparams )
    locs_map = stmv::array_map( "xy->1", locs, gridparams=p$gridparams )
    locs_index = match( locs_map, domain_map )
    if (! "POSIXct" %in% class(timestamp)  ) timestamp = as.POSIXct( timestamp, tz="America/Halifax", origin=lubridate::origin  )
    yrs = lubridate::year(timestamp)
    yrs_index = match( yrs, p$yrs )
    dyear = lubridate::decimal_date( timestamp ) - yrs
    dyear_index = as.numeric( cut( dyear, breaks=c(p$dyears, p$dyears[length(p$dyears)]+ diff(p$dyears)[1] ) , include.lowest=T, ordered_result=TRUE ) )
    dindex = cbind(locs_index, yrs_index, dyear_index ) # check this
    p$stmv_variables = NULL  # this can exist and cause confusion
    temp = temperature.db( p=p, DS="spatial.annual.seasonal")
    out = temp[dindex]
  return(out)
}


