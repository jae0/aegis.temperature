
temperature_carstm = function( p=NULL, DS="aggregated_data", id=NULL, sppoly=NULL, redo=FALSE, ... ) {

  #\\ Note inverted convention: depths are positive valued
  #\\ i.e., negative valued for above sea level and positive valued for below sea level
  if ( is.null(p)) p = temperature_parameters(...)

  if ( !exists("project_name", p)) p$project_name = "temperature"
  if ( !exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project_name )
  if ( !exists("datadir", p) )   p$datadir  = file.path( p$data_root, "data" )
  if ( !exists("modeldir", p) )  p$modeldir = file.path( p$data_root, "modelled" )


  if (!exists("areal_units_strata_type", p )) p$areal_units_strata_type = "lattice" #
  if (!exists("areal_units_constraint", p )) p$areal_units_constraint = "none" #
  if (!exists("areal_units_overlay", p )) p$areal_units_overlay = "none" #
  if (!exists("areal_units_resolution_km", p )) stop( "areal_units_resolution_km should be defined ... " ) # km
  if (!exists("areal_units_proj4string_planar_km", p )) stop( "areal_units_proj4string_planar_km should be defined ... " ) # km
  if (!exists("timeperiod", p) )  p$timeperiod="default"



  if (is.null(id)) id = paste( p$spatial_domain, paste0(p$areal_units_overlay, collapse="_"), p$areal_units_resolution_km, p$areal_units_strata_type, p$areal_units_constraint, p$timeperiod, sep="_" )


  # -----------------------


  if ( DS=="aggregated_data") {

    fn = file.path( p$modeldir, paste( "temperature", "aggregated_data", id, "rdata", sep=".") )
    if (!redo)  {
      print( "Warning: aggregated_data is loading from a saved instance ... add redo=TRUE if data needs to be refresh" )
      if (file.exists(fn)) {
        load( fn)
        return( sppoly )
      }
      print( "Warning: aggregated_data load from saved instance failed ... " )
    }

    print( "Warning: aggregated_data is being recreated ... " )

    M = temperature.db( p=p, DS="bottom.all"  )
    M = M[ which(M$yr %in% p$yrs), ]
    M$tiyr = lubridate::decimal_date ( M$date )

    # globally remove all unrealistic data
    keep = which( M$t >= -3 & M$t <= 25 ) # hard limits
    if (length(keep) > 0 ) M = M[ keep, ]
    TR = quantile(M$t, probs=c(0.0005, 0.9995), na.rm=TRUE ) # this was -1.7, 21.8 in 2015
    keep = which( M$t >=  TR[1] & M$t <=  TR[2] )
    if (length(keep) > 0 ) M = M[ keep, ]
    keep = which( M$z >=  2 ) # ignore very shallow areas ..
    if (length(keep) > 0 ) M = M[ keep, ]

    crs_lonlat = sp::CRS(projection_proj4string("lonlat_wgs84"))
    M$StrataID = over( SpatialPoints( M[, c("lon", "lat")], crs_lonlat ), spTransform(spp, crs_lonlat ) )$StrataID # match each datum to an area

    o = NULL
    M$lon = NULL
    M$lat = NULL
    gc()

    M$dyear = M$tiyr - M$yr

    dyear_discretization_rawdata = c( {c(1:365)-1} * p$inputdata_temporal_discretization_yr, 1)  # i.e., on a daily basis
    M$time_id = paste(M$yr, cut( M$dyear, breaks=p$dyear_discretization_rawdata, include.lowest=T, ordered_result=TRUE ), sep="_" )

    bb = as.data.frame( t( simplify2array(
      tapply( X=M$t, INDEX=list(paste( M$StrataID, M$time_id) ),
        FUN = function(w) { c(
          mean(w, na.rm=TRUE),
          sd(w, na.rm=TRUE),
          length( which(is.finite(w)) )
        ) }, simplify=TRUE )
    )))

    M = NULL
    colnames(bb) = c("temperature.mean", "temperature.sd", "temperature.n")
    bb$StrataID = rownames(bb)
    sppoly$t = NA
    sppoly$t.sd = NA
    sppoly$t.n = NA
    j = match( bb$StrataID, sppoly$StrataID )
    if (length(j) > 0)  {
      sppoly$t[j] = bb$t.mean
      sppoly$t.sd[j] = bb$t.sd
      sppoly$t.n[j] = bb$t.n
    }
    save( sppoly, file=fn, compress=TRUE )
    return( sppoly )
  }



  # ------------


  if ( DS=="carstm_inputs") {

    fn = file.path( p$modeldir, paste( "temperature", "carstm_inputs", id, "rdata", sep=".") )
    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        return( M )
      }
    }

    # prediction surface
    if (is.null(sppoly)) sppoly = areal_units( p=p )  # will redo if not found

    crs_lonlat = sp::CRS(projection_proj4string("lonlat_wgs84"))

    # do this immediately to reduce storage for sppoly (before adding other variables)

    M = bathymetry_carstm ( p=p, DS="aggregated_data" )  # 16 GB in RAM just to store!

    # reduce size
    M = M[ which( M$lon > p$corners$lon[1] & M$lon < p$corners$lon[2]  & M$lat > p$corners$lat[1] & M$lat < p$corners$lat[2] ), ]
    # levelplot(z.mean~plon+plat, data=M, aspect="iso")

    M$StrataID = over( SpatialPoints( M[, c("lon", "lat")], crs_lonlat ), spTransform(sppoly, crs_lonlat ) )$StrataID # match each datum to an area
    M$lon = NULL
    M$lat = NULL
    M$plon = NULL
    M$plat = NULL
    M = M[ which(is.finite(M$StrataID)),]
    M$StrataID = as.character( M$StrataID )  # match each datum to an area

    M$z = M$z.mean + p$constant_offset # make all positive
    M$tag = "observations"

    sppoly_df = as.data.frame(sppoly)
    sppoly_df$z = NA
    sppoly_df$StrataID = as.character( sppoly_df$StrataID )
    sppoly_df$tag ="predictions"

    vn = c("z", "tag", "StrataID")

    M = rbind( M[, vn], sppoly_df[, vn] )
    sppoly_df = NULL

    M$StrataID  = factor( as.character(M$StrataID), levels=levels( sppoly$StrataID ) ) # revert to factors
    sppoly = NULL
    M$strata  = as.numeric( M$StrataID)
    M$iid_error = 1:nrow(M) # for inla indexing for set level variation

    save( M, file=fn, compress=TRUE )
    return( M )
  }


  # ------------



  if ( DS=="carstm_modelled") {

    fn = file.path( p$modeldir, paste( "temperature", "carstm_modelled", id, p$carstm_modelengine, "rdata", sep=".") )
    fn_fit = file.path( p$modeldir, paste( "temperature", "carstm_modelled_fit", id, p$carstm_modelengine, "rdata", sep=".") )

    if (!redo)  {
      print( "Warning: carstm_modelled is loading from a saved instance ... add redo=TRUE if data needs to be refresh" )
      if (file.exists(fn)) {
        load( fn)
        return( sppoly )
      }
      if (DS=="carstm_modelled_fit") {
        if (file.exists(fn_fit)) {
          load( fn_fit )
          return( fit )
        }
      }
      print( "Warning: carstm_modelled load from saved instance failed ... " )
    }

    print( "Warning: carstm_modelled is being recreated ... " )
    print( "Warning: this needs a lot of RAM .. ~XX GB depending upon resolution of discretization .. a few hours " )

    # prediction surface
    if (is.null(sppoly))  sppoly = areal_units( p=p )  # will redo if not found

    M = temperature.db ( p=p, DS="lonlat.highres" )  # 16 GB in RAM just to store!
    crs_lonlat = sp::CRS("+proj=longlat +datum=WGS84")
    spp = sppoly["StrataID"]

    # do this immediately to reduce storage for spp (before adding other variables)
    M$StrataID = as.character( over( SpatialPoints( M[, c("lon", "lat")], crs_lonlat ), spTransform(spp, crs_lonlat ) )$StrataID) # match each datum to an area
    o = NULL
    M$lon = NULL
    M$lat = NULL
    # M$temperature
    M$tag = "observations"

    spp = as.data.frame(spp)
    spp$temperature = NA
    spp$StrataID = as.character( spp$StrataID )
    spp$tag ="predictions"

    M = rbind( M, spp[, names(M)] )
    M$StrataID  = factor( as.character(M$StrataID), levels=levels( sppoly$StrataID ) ) # revert to factors
    M$strata  = as.numeric( M$StrataID)
    M$iid_error = 1:nrow(M) # for inla indexing for set level variation

    spp = NULL
    gc()

    B = aegis.bathymetry::bathymetry_carstm( p=p, DS="carstm_modelled" )
    ddepths = c(2.5, 5, 10, 20, 40, 80, 160, 320, 640 )
    M$depth = as.numeric( as.character( cut( M$depth, breaks=ddepths, labels=diff(ddepths)/2 + ddepths[-length(ddepths)], include.lowest=TRUE ) ))

    mm = match( as.character(M$StrataID), as.character(B$StrataID) )
    tokeep = c("depth", "depth.sd")
    M = cbind(M, B[mm, tokeep])

    H = carstm_hyperparameters( sd(log(M$temperature), na.rm=TRUE), alpha=0.5, median( log(M$temperature), na.rm=TRUE) )
    fit = inla(
      formula =
        temperature ~ 1 +
          + f(strata, model="bym2", graph=sppoly@nb, scale.model=TRUE, constr=TRUE, hyper=H$bym2)
          + f(depth, model="rw2", hyper=H$rw2)
          + f(depth.sd, model="rw2", hyper=H$rw2)
          + f(iid_error, model="iid", hyper=H$iid)
        ,
      family = "lognormal",
      data= M,
      control.compute=list(dic=TRUE, config=TRUE),
      control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
      control.predictor=list(compute=FALSE, link=1 ),
      # control.fixed=H$fixed,  # priors for fixed effects, generic is ok
      # control.inla=list(int.strategy="eb") ,# to get empirical Bayes results much faster.
      # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
      num.threads=2,
      blas.num.threads=2,
      verbose=TRUE
    )
    s = summary(fit)
    s$dic$dic  # 31225
    s$dic$p.eff # 5200

    plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

    # reformat predictions into matrix form
    ii = which(M$tag=="predictions")
    jj = match(M$StrataID[ii], sppoly$StrataID)
    sppoly@data$temperature.predicted = exp( fit$summary.fitted.values[ ii[jj], "mean" ]) - 2500
    sppoly@data$temperature.predicted_lb = exp( fit$summary.fitted.values[ ii[jj], "0.025quant" ]) - 2500
    sppoly@data$temperature.predicted_ub = exp( fit$summary.fitted.values[ ii[jj], "0.975quant" ]) - 2500
    sppoly@data$temperature.random_strata_nonspatial = exp( fit$summary.random$strata[ jj, "mean" ])
    sppoly@data$temperature.random_strata_spatial = exp( fit$summary.random$strata[ jj+max(jj), "mean" ])
    sppoly@data$temperature.random_sample_iid = exp( fit$summary.random$iid_error[ ii[jj], "mean" ])

    attr( spplot, "fit") = fit
    save( spplot, file=fn, compress=TRUE )

    vn = "temperature.predicted"
    brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
    dev.new()
    spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )

    return( sppoly )

  }

}
