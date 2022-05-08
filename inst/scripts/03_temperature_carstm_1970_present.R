
    require(aegis.temperature)

    year.assessment = 2021
    
    # about 24 hrs for 1999:2021

    # used by aegis.survey (cod, wolfish, etc)  -- about 16 hrs to model 3+hr to plot
    # the theta are starting points for the hyper params on link scale
    p = temperature_parameters( 
      project_class="carstm", 
      yrs=1970:year.assessment, 
      carstm_model_label="1970_present", 
      theta = c(-0.79, 0.83, 1.35, 0.88, -2.53, 6.89, -4.01, -0.84, 22.22, 1.33 ) 
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
    xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if input data has changed
    
    xydata=temperature_db(p=p, DS="areal_units_input" )  # redo if input data has changed
    xydata = xydata[ which(xydata$yr %in% p$yrs), ]
    sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create

    sppoly = areal_units( p=p, xydata=xydata  )  # to force create
    
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
    posterior_simulations_to_retain="predictions", 
    num.threads="4:2",  # adjust for your machine
    mc.cores=2,
    # if problems, try any of: 
    # control.inla = list( strategy='laplace'),
    # control.inla = list( strategy='adaptive', int.strategy="eb" ),
    redo_fit=TRUE,
    # debug = TRUE,
    # debug = "random_covariates",
    verbose=TRUE 
  )    



## --- long run results: 1970-present

# Deviance Information Criterion (DIC) ...............: 1029074.48
# Deviance Information Criterion (DIC, saturated) ....: 4380433.99
# Effective number of parameters .....................: 25496.71

# Watanabe-Akaike information criterion (WAIC) ...: 1031320.56
# Effective number of parameters .................: 24744.12

# Marginal log-Likelihood:  -493738.48 
# Posterior summaries for the linear predictor and the fitted values are computed
# (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

#    --- NOTE: parameter estimates are on link scale and not user scale.

#    --- NOTE: even if the model fit completes, failure of extraction is possible when NAN or INF are encountered... this means that your model / data probably needs tbe changed.


# Computing summaries and computing from posterior simulations (can be longer than model fitting depending upon no of posterior sims: 'nposteriors' ) ...
# Extracting parameter summaries

# Fixed effects
#                mean       sd quant0.025 quant0.5 quant0.975   parameter
# (Intercept) 6.12952 0.121981    5.89146  6.12853    6.36863 (Intercept)


# Random effects:
#                                                   mean          sd quant0.025 quant0.5 quant0.975
# SD the Gaussian observations                  1.489636 0.002316616   1.485455 1.489485   1.494507
# SD time                                       0.654675 0.058398555   0.544248 0.653379   0.773209
# SD cyclic                                     0.598698 0.080189860   0.459342 0.592056   0.773596
# SD space                                      3.592843 0.081680576   3.469444 3.580121   3.780965
# SD inla.group(z, method = "quantile", n = 11) 8.179492 1.064618633   6.264249 8.118804  10.438437
# SD space_time                                 1.533367 0.014816895   1.508963 1.531631   1.566200
# Rho for time                                  0.533253 0.088503469   0.339906 0.540554   0.685016
# GroupRho for space_time                       0.576076 0.012911940   0.546949 0.577741   0.596559
# Phi for space                                 0.998661 0.000381386   0.997762 0.998715   0.999242
# Phi for space_time                            1.000000 0.000000000   1.000000 1.000000   1.000000


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
    type="b", ylim=c(-2, 2), xlab="Year", ylab="Botton temperature (Celsius)", h=7  )

  carstm_plotxy( res, vn=c( "res", "random", "cyclic" ), 
    type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(-1.5, 1.5),
    xlab="Season", ylab="Botton temperature (Celsius)", h=7  )

  carstm_plotxy( res, vn=c( "res", "random", "inla.group(z, method = \"quantile\", n = 11)" ), 
    type="b", col="slategray", pch=19, lty=1, lwd=2.5, ylim=c(-2, 2) ,
    xlab="Depth (m)", ylab="Botton temperature (Celsius)", h=7  )




 
  # maps of some of the results
  tmatch="2021"
  umatch="0.75"

  tmout = carstm_map(  res=res, vn="predictions", tmatch=tmatch, umatch=umatch, 
    sppoly=sppoly,
    breaks=seq(-1, 9, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
    title=paste( "Bottom temperature predictions", tmatch, umatch)  
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space", "combined" ), 
    sppoly=sppoly,
    breaks=seq(-5, 5, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
    title="Bottom temperature spatial effects (Celsius)"
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
    sppoly=sppoly,
    breaks=seq(-2, 2, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
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
    title=paste( "Bottom temperature spatiotemporal effects", tmatch, umatch)  
  )
  tmout
 
  graphics.off()

  # map all bottom temps:
  outputdir = file.path(p$data_root, "maps", p$carstm_model_label )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )



  # bbox = c(-71.5, 41, -52.5,  50.5 )
  additional_features = additional_features_tmap( 
      p=p, 
      isobaths=c( 10, 100, 200, 300, 500, 1000 ), 
      coastline =  c("canada"), 
      xlim=c(-80,-40), 
      ylim=c(38, 60) 
  )
  
  
  fn_root = paste("Predicted_habitat_probability_persistent_spatial_effect", sep="_")
  outfilename = file.path( outputdir, paste(fn_root, "png", sep=".") )
  
  vn = c( "random", "space", "combined" ) 
  
  toplot = carstm_results_unpack( res, vn )
  brks = pretty(  quantile(toplot[,"mean"], probs=c(0,0.975), na.rm=TRUE )  )

  tmout = carstm_map(  res=res, vn=vn, 
    sppoly = sppoly, 
    breaks = brks,
    palette="-RdYlBu",
    plot_elements=c(  "compass", "scale_bar", "legend" ),
    additional_features=additional_features,
    title= "Bottom temperature -- persistent spatial effect" ,
    outfilename=outfilename
  )  
  tmout



  # slow due to use of webshot to save html to png (partial solution until tmap view mode saves directly)
  brks = pretty(c(1, 9))
  for (y in res$time ){
    for ( u in res$cyclic  ){
      fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
      outfilename = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
      tmout = carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
        breaks=brks, 
        sppoly=sppoly,
        palette="-RdYlBu",
        plot_elements=c(  "compass", "scale_bar", "legend" ),
        additional_features=additional_features,
        title=fn_root,  
        outfilename=outfilename
      )
    }
  }
# end

 