
  require(aegis.temperature)

  year.assessment = 2021
  
  # about 24 hrs for 1999:2021

    # used by aegis.survey (cod, wolfish, etc)  -- about 16 hrs to model 3+hr to plot
    # the theta are starting points for the hyper params on link scale
    p = temperature_parameters( 
      project_class="carstm", 
      yrs=1970:year.assessment, 
      carstm_model_label="1970_present", 
      theta = c( -0.887, 0.678, 1.573, 0.563, -2.321, 6.928, -4.056, -0.747, 21.491, 1.216   ) 
    ) 
   
    # List of hyperparameters: 
    # theta[0] = [Log precision for the Gaussian observations]
		# theta[1] = [Log precision for time]
		# theta[2] = [Rho_intern for time]
		# theta[3] = [Log precision for cyclic]
		# theta[4] = [Log precision for space]
		# theta[5] = [Logit phi for space]
		# theta[6] = [Log precision for inla.group(z, method = "quantile", n = 11)]
		# theta[7] = [Log precision for space_time]
		# theta[8] = [Logit phi for space_time]
		# theta[9] = [Group rho_intern for space_time]
 

    # to recreate the underlying data
    xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed
    
    xydata=temperature_db(p=p, DS="areal_units_input" )  # redo if inpute data has changed
    xydata = xydata[ which(xydata$yr %in% p$yrs), ]
    sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create

    plot( sppoly[ "npts" ] ) 
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )   # or using carstm_map:
    

  # ------------------------------
  # prep data
  sppoly = areal_units( p=p  )  # same
  
  M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE )  # must  redo if sppoly has changed or new data
  M = NULL


  # !!! WARNING: this uses a lot of RAM  
  fit = NULL

  fit = carstm_model( 
    p=p, 
    data ='temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly )',  
    sppoly=sppoly,
    num.threads="4:2",  # adjust for your machine
    mc.cores=2,
    # if problems, try any of: 
    # control.inla = list( strategy='laplace'),
    # control.inla = list( strategy='adaptive', int.strategy="eb" ),
    # redo_fit=TRUE, # to start optim from a solution close to the final in 2021 ... 
    redo_fit=TRUE, # to start optim from a solution close to the final in 2021 ... 
    # debug = TRUE,
    # debug = "random_covariates",
    verbose=TRUE 
  )    



## --- long run results: 1970-present

  # Deviance Information Criterion (DIC) ...............: 1030247.34
  # Deviance Information Criterion (DIC, saturated) ....: 4326334.48
  # Effective number of parameters .....................: 21323.92

  # Watanabe-Akaike information criterion (WAIC) ...: 1031476.32
  # Effective number of parameters .................: 20380.37

  # Marginal log-Likelihood:  -492527.45 
  # Posterior summaries for the linear predictor and the fitted values are computed
  # (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

  #   --- NOTE: parameter estimates are on link scale and not user scale.

  #   --- NOTE: even if the model fit completes, failure of extraction is possible when NAN or INF are encountered... this means that your model / data probably needs tbe changed.


  # Computing summaries and computing from posterior simulations (can be longer than model fitting depending upon no of posterior sims: 'nposteriors' ) ...
  # Extracting parameter summaries

  # Fixed effects
  #               mean       sd quant0.025 quant0.5 quant0.975   parameter
  # (Intercept) 6.19742 0.143409    5.91599    6.197    6.47802 (Intercept)

  # Model may be over parameterized. NAN and Inf values encountered in phis. Try alt parameterizations or smaller number of n or masking negative values

  # Random effects:
  #                                                   mean         sd quant0.025 quant0.5 quant0.975
  # SD the Gaussian observations                   1.562947 0.00345120   1.555127 1.563424   1.568302
  # SD time                                        0.669060 0.00936281   0.654173 0.667805   0.690158
  # SD cyclic                                      0.830288 0.04820958   0.765649 0.820649   0.946243
  # SD space                                       3.250345 0.05948477   3.152214 3.243385   3.382347
  # SD inla.group(z, method = "quantile", n = 11) 11.427465 5.44706444   5.930292 9.810387  26.121735
  # SD space_time                                  1.457522 0.01163610   1.433461 1.458056   1.479000
  # Rho for time                                   0.377329 0.02457540   0.321641 0.380658   0.415802
  # GroupRho for space_time                        0.539559 0.01036008   0.519184 0.539531   0.559783



 
      
    # extract results
    if (0) {
      # very large files .. slow 
      fit = carstm_model( p=p, DS="carstm_modelled_fit", sppoly=sppoly )  # extract currently saved model fit
      fit$summary$dic$dic
      fit$summary$dic$p.eff
    
      plot(fit)
      plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
    }


  res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  ) # to load currently saved results


  b0 = res$summary$fixed_effects["(Intercept)", "mean"]

  ts =  res$random$time 
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] 
  plot( mean ~ ID, ts, type="b", ylim=c(-2,2), lwd=1.5, xlab="year")
  lines( quant0.025 ~ ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ ID, ts, col="gray", lty="dashed")


  ts =  res$random$cyclic
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] 
  plot( mean ~ID, ts, type="b", ylim=c(-1.5, 1.5), lwd=1.5, xlab="fractional year")
  lines( quant0.025 ~ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ID, ts, col="gray", lty="dashed")



  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7

  # maps of some of the results
  tmatch="1970"
  umatch="0.15"

  tmout = carstm_map(  res=res, vn="predictions", tmatch=tmatch, umatch=umatch, 
    sppoly=sppoly,
    breaks=seq(-1, 9, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature predictions", tmatch, umatch)  
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space", "combined" ), 
    sppoly=sppoly,
    breaks=seq(-5, 5, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title="Bottom temperature spatial effects (Celcius)"
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
    sppoly=sppoly,
    breaks=seq(-2, 2, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature spatiotemporal effects", tmatch, umatch)  
  )
  tmout


  tmatch="2020"
  umatch="0.15"

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
    sppoly=sppoly,
    breaks=seq(-2, 2, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature spatiotemporal effects", tmatch, umatch)  
  )
  tmout

  # map all bottom temps:
  outputdir = file.path(p$data_root, "maps", p$carstm_model_label )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  graphics.off()
  # bbox = c(-71.5, 41, -52.5,  50.5 )


  background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 

  # slow due to use of webshot to save html to png (partial solution until tmap view mode saves directly)
  for (y in res$time ){
    for ( u in res$cyclic ){
      fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
      outfilename = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
      tmout = carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
        breaks=seq( 1, 9), 
        sppoly=sppoly,
        palette="-RdYlBu",
        title=fn_root,  
        plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
        background = background,
        map_mode="view",
        tmap_zoom= c(map_centre, map_zoom)
      )
      mapview::mapshot( tmap_leaflet(tmout), file=outfilename, vwidth = 1600, vheight = 1200 )
      print( outfilename )
    }
  }
# end

 