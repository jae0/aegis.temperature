
temperature_carstm = function( p=NULL, DS=NULL, sppoly=NULL, redo=FALSE, ... ) {

  #\\ Note inverted convention: depths are positive valued
  #\\ i.e., negative valued for above sea level and positive valued for below sea level

  # deal with additional passed parameters
  if ( is.null(p) ) p=list()
  p_add = list(...)
  if (length(p_add) > 0 ) p = c(p, p_add)
  i = which(duplicated(names(p), fromLast = TRUE ))
  if ( length(i) > 0 ) p = p[-i] # give any passed parameters a higher priority, overwriting pre-existing variable


  # -----------------------


  if ( DS=="carstm_inputs") {

    fn = file.path( p$modeldir, paste( "temperature", "carstm_inputs", p$auid, "rdata", sep=".") )
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

    M = temperature.db ( p=p, DS="aggregated_data" )  # 16 GB in RAM just to store!

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

    M$t = M$t.mean
    M$tag = "observations"

    APS = as.data.frame(sppoly)
    APS$t = NA
    APS$StrataID = as.character( APS$StrataID )
    APS$tag ="predictions"
    APS$z = NA
    APS$z = match(M, B)

    vn = c("t", "tag", "StrataID", "z")
    APS = APS[, vn]

    n_aps = nrow(APS)
    APS = cbind( APS[ rep.int(1:n_aps, p$nt), ],
                      rep.int( p$prediction_ts, rep(n_aps, p$nt )) )
    names(APS) = c(vn, "tiyr")

    M = rbind( M[, names(APS)], APS )
    APS = NULL

    M$StrataID  = factor( as.character(M$StrataID), levels=levels( sppoly$StrataID ) ) # revert to factors
    sppoly = NULL
    M$strata  = as.numeric( M$StrataID)
    M$iid_error = 1:nrow(M) # for inla indexing for set level variation

    save( M, file=fn, compress=TRUE )
    return( M )
  }


  # ------------



  if ( DS=="carstm_modelled") {

    fn = file.path( p$modeldir, paste( "temperature", "carstm_modelled", p$auid, p$carstm_modelengine, "rdata", sep=".") )
    fn_fit = file.path( p$modeldir, paste( "temperature", "carstm_modelled_fit", p$auid, p$carstm_modelengine, "rdata", sep=".") )

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

    M$depth = discretize_data( M$depth, p$discretization$z )

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
