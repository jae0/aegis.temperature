
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

    fn = file.path( p$modeldir, paste( "temperature", "carstm_inputs", p$auid,
      p$inputdata_spatial_discretization_planar_km,
      round(p$inputdata_temporal_discretization_yr, 6),
      "rdata", sep=".") )

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
    APS$StrataID = as.character( APS$StrataID )
    APS$tag ="predictions"
    APS$t = NA
    APS$z = NA

    pb = p
    pb$modeldir = NULL  # resetting forces default bathymetry model dir to be used
    pb$project_name = NULL
    pb$data_root = NULL
    pb$datadir  = NULL
    pb = bathymetry_parameters(p=pb, DS="carstm")
    BI = bathymetry_carstm ( p=pb, DS="carstm_modelled" )  # unmodeled!
    jj = match( as.character( APS$StrataID), as.character( BI$StrataID) )
    APS$z = BI$z.predicted[jj]
    jj =NULL
    BI = NULL

    vn = c("t", "tag", "StrataID", "z")
    APS = APS[, vn]

    # expand APS to all time slices
    n_aps = nrow(APS)
    APS = cbind( APS[ rep.int(1:n_aps, p$nt), ], rep.int( p$prediction_ts, rep(n_aps, p$nt )) )
    names(APS) = c(vn, "tiyr")

    M$temperature = M$temperature.mean
    M$tiyr = M$yr + M$dyear
    M = rbind( M[, names(APS)], APS )
    APS = NULL

    M$StrataID  = factor( as.character(M$StrataID), levels=levels( sppoly$StrataID ) ) # revert to factors
    sppoly = NULL

    save( M, file=fn, compress=TRUE )
    return( M )
  }


  # ------------



  if ( DS=="carstm_modelled") {

    auids = paste(  p$carstm_modelengine, p$auid, p$inputdata_spatial_discretization_planar_km,
      round(p$inputdata_temporal_discretization_yr, 6),   sep="_" )

    fn = file.path( p$modeldir, paste("temperature", "carstm_modelled", auids, "rdata", sep="." )
    fn_fit = file.path( p$modeldir, paste( "temperature", "carstm_modelled_fit", p$auid, p$carstm_modelengine, "rdata", sep=".") )

    if (!redo)  {
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
    }

       # prediction surface
    if (is.null(sppoly))  sppoly = areal_units( p=p )  # will redo if not found

    M = temperature_carstm ( p=p, DS="carstm_inputs" )  # 16 GB in RAM just to store!

    fit  = NULL

    if ( grepl("glm", p$carstm_modelengine) ) {

      assign("fit", eval(parse(text=paste( "try(", p$carstm_modelcall, ")" ) ) ))
      if (is.null(fit)) error("model fit error")
      if ("try-error" %in% class(fit) ) error("model fit error")
      save( fit, file=fn_fit, compress=TRUE )

      # s = summary(fit)
      # AIC(fit)  # 104487274
      # reformat predictions into matrix form
      ii = which( M$tag=="predictions" & M$StrataID %in% M[ which(M$tag=="observations"), "StrataID"] )
      preds = predict( fit, newdata=M[ii,], type="link", na.action=na.omit, se.fit=TRUE )  # no/km2

      # out = reformat_to_matrix(
      #   input = preds$fit,
      #   matchfrom = list( StrataID=M$StrataID[ii] ),
      #   matchto   = list( StrataID=sppoly$StrataID  )
      # )
      # iy = match( as.character(sppoly$StrataID), aps$StrataID )
        sppoly@data[,"temperature.predicted"] = exp( preds$fit)
        sppoly@data[,"temperature.predicted_se"] = exp( preds$se.fit)
        sppoly@data[,"temperature.predicted_lb"] = exp( preds$fit - preds$se.fit )
        sppoly@data[,"temperature.predicted_ub"] = exp( preds$fit + preds$se.fit )
        save( sppoly, file=fn, compress=TRUE )
      }

      if ( grepl("gam", p$carstm_modelengine) ) {
        assign("fit", eval(parse(text=paste( "try(", p$carstm_modelcall, ")" ) ) ))
        if (is.null(fit)) error("model fit error")
        if ("try-error" %in% class(fit) ) error("model fit error")
        save( fit, file=fn_fit, compress=TRUE )

        s = summary(fit)
        AIC(fit)  # 104487274
        # reformat predictions into matrix form
        ii = which( M$tag=="predictions" & M$StrataID %in% M[ which(M$tag=="observations"), "StrataID"] )
        preds = predict( fit, newdata=M[ii,], type="link", na.action=na.omit, se.fit=TRUE )  # no/km2
        sppoly@data[,"temperature.predicted"] = exp( preds$fit)
        sppoly@data[,"temperature.predicted_se"] = exp( preds$se.fit)
        sppoly@data[,"temperature.predicted_lb"] = exp( preds$fit - preds$se.fit )
        sppoly@data[,"temperature.predicted_ub"] = exp( preds$fit + preds$se.fit )
        save( sppoly, file=fn, compress=TRUE )
    }


    if ( grepl("inla", p$carstm_modelengine) ) {

      H = carstm_hyperparameters( sd(M$temperature, na.rm=TRUE), alpha=0.5, median( M$temperature, na.rm=TRUE) )

      M$zi = discretize_data( M$z, p$discretization$z )
      M$tiyr2 = M$tiyr  # use a copy for "seasonal" models
      M$year = floor(M$tiyr)
      M$dyear  =  M$tiyr - M$year
      M$strata  = as.numeric( M$StrataID)
      M$iid_error = 1:nrow(M) # for inla indexing for set level variation

      assign("fit", eval(parse(text=paste( "try(", p$carstm_modelcall, ")" ) ) ))
      if (is.null(fit)) error("model fit error")
      if ("try-error" %in% class(fit) ) error("model fit error")
      save( fit, file=fn_fit, compress=TRUE )

      s = summary(fit)
      s$dic$dic  # 31225
      s$dic$p.eff # 5200

      plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

      # reformat predictions into matrix form
      ii = which(M$tag=="predictions")
      jj = match(M$StrataID[ii], sppoly$StrataID)
      sppoly@data$temperature.predicted = exp( fit$summary.fitted.values[ ii[jj], "mean" ])
      sppoly@data$temperature.predicted_lb = exp( fit$summary.fitted.values[ ii[jj], "0.025quant" ])
      sppoly@data$temperature.predicted_ub = exp( fit$summary.fitted.values[ ii[jj], "0.975quant" ])
      sppoly@data$temperature.random_strata_nonspatial = exp( fit$summary.random$strata[ jj, "mean" ])
      sppoly@data$temperature.random_strata_spatial = exp( fit$summary.random$strata[ jj+max(jj), "mean" ])
      sppoly@data$temperature.random_sample_iid = exp( fit$summary.random$iid_error[ ii[jj], "mean" ])
      save( sppoly, file=fn, compress=TRUE )
    }


    if (map_results) {
      vn = "temperature.predicted"
      brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
      dev.new();  spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )
    }

    return( sppoly )

  }

  #   fit = inla(
  #     formula =
  #       temperature ~ 1 +
  #         + f(strata, model="bym2", graph=sppoly@nb, scale.model=TRUE, constr=TRUE, hyper=H$bym2)
  #         + f(depth, model="rw2", hyper=H$rw2)
  #         + f(depth.sd, model="rw2", hyper=H$rw2)
  #         + f(iid_error, model="iid", hyper=H$iid)
  #       ,
  #     family = "lognormal",
  #     data= M,
  #     control.compute=list(dic=TRUE, config=TRUE),
  #     control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
  #     control.predictor=list(compute=FALSE, link=1 ),
  #     # control.fixed=H$fixed,  # priors for fixed effects, generic is ok
  #     # control.inla=list(int.strategy="eb") ,# to get empirical Bayes results much faster.
  #     # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
  #     num.threads=2,
  #     blas.num.threads=2,
  #     verbose=TRUE
  #   )
  #   s = summary(fit)
  #   s$dic$dic  # 31225
  #   s$dic$p.eff # 5200

  #   plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )

  #   # reformat predictions into matrix form
  #   ii = which(M$tag=="predictions")
  #   jj = match(M$StrataID[ii], sppoly$StrataID)
  #   sppoly@data$temperature.predicted = exp( fit$summary.fitted.values[ ii[jj], "mean" ]) - 2500
  #   sppoly@data$temperature.predicted_lb = exp( fit$summary.fitted.values[ ii[jj], "0.025quant" ]) - 2500
  #   sppoly@data$temperature.predicted_ub = exp( fit$summary.fitted.values[ ii[jj], "0.975quant" ]) - 2500
  #   sppoly@data$temperature.random_strata_nonspatial = exp( fit$summary.random$strata[ jj, "mean" ])
  #   sppoly@data$temperature.random_strata_spatial = exp( fit$summary.random$strata[ jj+max(jj), "mean" ])
  #   sppoly@data$temperature.random_sample_iid = exp( fit$summary.random$iid_error[ ii[jj], "mean" ])

  #   attr( spplot, "fit") = fit
  #   save( spplot, file=fn, compress=TRUE )

  #   vn = "temperature.predicted"
  #   brks = interval_break(X= sppoly[[vn]], n=length(p$mypalette), style="quantile")
  #   dev.new()
  #   spplot( sppoly, vn, col.regions=p$mypalette, main=vn, at=brks, sp.layout=p$coastLayout, col="transparent" )

  #   return( sppoly )

  # }

}
