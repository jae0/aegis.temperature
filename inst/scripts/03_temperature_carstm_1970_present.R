
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
    # NOTE:: this reshapes the geographic boundaries to make things a bit more compact 
    # .... will need to alter to get more expansive results
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
    control.inla = list( strategy='adaptive', int.strategy="eb" ),
    # redo_fit=TRUE, # to start optim from a solution close to the final in 2021 ... 
    redo_fit=TRUE, # to start optim from a solution close to the final in 2021 ... 
    # debug = TRUE,
    # debug = "random_covariates",
    verbose=TRUE 
  )    



## --- long run results: 1970-present
# Deviance Information Criterion (DIC) ...............: 1028957.59
# Deviance Information Criterion (DIC, saturated) ....: 4345288.12
# Effective number of parameters .....................: 24526.26

# Watanabe-Akaike information criterion (WAIC) ...: 1030893.52
# Effective number of parameters .................: 23671.73

# Marginal log-Likelihood:  -493362.38 
# Fixed effects
#                mean      sd quant0.025 quant0.5 quant0.975   parameter
# (Intercept) 6.25488 0.12336    6.01278  6.25451    6.49624 (Intercept)

# Model may be over parameterized. NAN and Inf values encountered in phis. Try alt parameterizations or smaller number of n or masking negative values

# Random effects:
#                                                   mean          sd quant0.025 quant0.5 quant0.975
# SD the Gaussian observations                  1.533401 2.28628e-03   1.529154 1.533297   1.538112
# SD time                                       0.591332 1.12385e-02   0.574946 0.589442   0.617457
# SD cyclic                                     0.513943 1.74034e-02   0.474958 0.516244   0.541037
# SD space                                      3.564353 5.68494e-02   3.463918 3.559885   3.685789
# SD inla.group(z, method = "quantile", n = 11) 6.778496 1.60957e+00   4.458754 6.478165  10.697137
# SD space_time                                 1.556909 1.13744e-02   1.534090 1.557107   1.578724
# Rho for time                                  0.384418 2.90973e-02   0.317340 0.388903   0.427952
# GroupRho for space_time                       0.594716 9.08126e-03   0.576733 0.594742   0.612322
# Phi for space                                      NaN         NaN   0.000000 0.000000        NaN
# Phi for space_time                            1.000000 9.94031e-08   1.000000 1.000000   1.000000
 
 
      
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



  carstm_plotxy( res, vn=c( "res", "random", "time" ), 
    type="b", ylim=c(0, 10), xlab="Year", ylab="Botton temperature (Celcius)", h=7  )

  carstm_plotxy( res, vn=c( "res", "random", "cyclic" ), 
    type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(-1.5, 1.5),
    xlab="Season", ylab="Botton temperature (Celcius)", h=7  )

  carstm_plotxy( res, vn=c( "res", "random", "inla.group(z, method = \"quantile\", n = 11)" ), 
    type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(0, 10) ,
    xlab="Depth (m)", ylab="Botton temperature (Celcius)", h=7  )





  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7

  # maps of some of the results
  tmatch="2021"
  umatch="0.75"

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
  umatch="0.75"

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
    for ( u in res$cyclic[6:8] ){
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

 