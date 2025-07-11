
# must contain "temperature", single square brackets as we need the name of the list kept
# plu = p$carstm_prediction_surface_parameters["temperature"] 

degree_days = function( plu, pg, years, t0) {
    require(aegis.temperature)
    year.assessment = max(years)

    M = st_drop_geometry(pg)
    setDT(M)
    M$AUID = as.character( M$AUID )
    M$t = NA
    nau = nrow(M)

    ny = length(years) # years for estimation of degree days
    nw = plu[["temperature"]][["nw"]] # use the same seasonal units as lookup table

    nt = nw * ny # i.e., seasonal with nw (default is annual: nt=ny)
    tres = 1 / nw # time resolution .. predictions are made with models that use seasonal components

    dyears = discretize_data( span=c(0, 1, nw), toreturn="lower" )  # left breaks .. (c(1:nw)-1) / nw # intervals of decimal years... fractional year breaks
    cyclic_levels = factor( discretize_data( span=c(0, 1, nw), toreturn="midpoints" ), ordered=TRUE )
 
    # predictions at these time values (decimal-year), # output timeslices for predictions in decimal years, yes all of them here
    tout = expand.grid( yr=years, dyear=discretize_data( span=c(0, 1, nw), toreturn="midpoints" ), KEEP.OUT.ATTRS=FALSE )
    prediction_ts = sort( tout$yr + tout$dyear  ) # mid-points

    # time and cyclic
    M = cbind( M[ rep.int(1:nau, nt), ], rep.int( prediction_ts, rep(nau, nt )) )
    names(M)[ncol(M)] = "tiyr"
    M$timestamp = lubridate::date_decimal( M$tiyr, tz=plu$timezone )
    M$year = trunc( M$tiyr )
    M$dyear = M$tiyr - M$year

    pL = aegis.temperature::temperature_parameters( project_class="carstm", carstm_model_label="default" , yrs=p$yrs )

    LUT= aegis_survey_lookuptable( aegis_project="temperature", 
        project_class="carstm", DS="carstm_predictions", pL=pL )

    M$t = aegis_lookup(
        pL=pL, LUT=LUT,
        ## name of the list kept  ----> here!
        LOCS=M[ , c("AUID", "timestamp")],
        LOCS_AU=pg,
        project_class = "carstm", # lookup from modelled predictions from carstm
        output_format = "areal_units",
        variable_name= list("predictions") ,
        statvars=c("mean"),
        space_resolution=plu$pres ,
        year.assessment=year.assessment,
        tz="America/Halifax",
        returntype = "vector"
    )

    # just in case missing in input data, generate and clean up
    M$tiyr = lubridate::decimal_date ( M$timestamp )
    M$tiyr = trunc( M$tiyr / tres )*tres # discretize for inla .. midpoints
    M$yr = trunc( M$tiyr)

    # do not separate out as season can be used even if not predicted upon
    ii = which( M$dyear > 1)
    if (length(ii) > 0) M$dyear[ii] = 0.99 # cap it .. some surveys go into the next year

    M$dyri = discretize_data( x=M[["dyear"]], span=c(0, 1, nw) )
    
    M$space = match( M$AUID, sppoly$AUID ) # for bym/car .. must be numeric index matching neighbourhood graphs
    M$time  = match( M$year, years ) # copy time for space_time component .. for groups, must be numeric index
    M$cyclic = match( M$dyri, discretize_data( span=c( 0, 1, nw) ) )  # as integer

    return(M)
}
