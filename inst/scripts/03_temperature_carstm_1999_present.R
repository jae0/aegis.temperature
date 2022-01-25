  # 2022-01-14 14:17:20


  require(aegis.temperature)

  year.assessment = 2021

  # about 24 hrs for 1999:2021
  # used by bio.snowcrab  --- about 24 hrs ( ~8 hrs + ;  6+ for mapping )... 
  # the theta are starting points for the hyper params on link scale
  p = temperature_parameters( 
    project_class="carstm", 
    yrs=1999:year.assessment, 
    carstm_model_label="1999_present",
    theta = c( -0.837, -0.270, -0.961, 2.389, -0.413, 4.492, -1.805, -0.454, 23.391, 0.789 ) 
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
    xydata = xydata[ which(xydata$yr %in% p$yrs), ]
    sppoly = areal_units( p=p, xydata=xydata, hull_alpha=20, redo=TRUE )  # to force create

    plot( sppoly[ "AUID" ] ) 
    
    # or using carstm_map: 
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot




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
    num.threads="6:2",  # adjust for your machine
    mc.cores=2,
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    control.inla = list( strategy='adaptive', int.strategy="eb" ),
    verbose=TRUE 
  )    


      
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

  ( res$summary)

  # $fixed_effects
  #                mean       sd quant0.025 quant0.5 quant0.975   parameter
  # (Intercept) 7.19757 0.231412    6.74317  7.19718    7.65117 (Intercept)

  # $random_effects
  #                                                   mean         sd quant0.025 quant0.5 quant0.975
  # SD the Gaussian observations                  1.444302 0.00272497   1.438668 1.444423   1.449339
  # SD time                                       0.799841 0.02516521   0.763887 0.795395   0.858967
  # SD cyclic                                     0.262678 0.00855010   0.244700 0.263244   0.277970
  # SD space                                      2.479560 0.05698903   2.373245 2.477228   2.596816
  # SD inla.group(z, method = "quantile", n = 11) 3.533001 1.03773735   2.350499 3.256218   6.246516
  # SD space_time                                 1.309558 0.01547451   1.279835 1.309265   1.340582
  # Rho for time                                  0.319038 0.04883276   0.205123 0.327166   0.389866
  # GroupRho for space_time                       0.375820 0.01859616   0.336755 0.376871   0.409390
  #                                                                                   parameter
  # SD the Gaussian observations                                   SD the Gaussian observations
  # SD time                                                                             SD time
  # SD cyclic                                                                         SD cyclic
  # SD space                                                                           SD space
  # SD inla.group(z, method = "quantile", n = 11) SD inla.group(z, method = "quantile", n = 11)
  # SD space_time                                                                 SD space_time
  # Rho for time                                                                   Rho for time
  # GroupRho for space_time                                             GroupRho for space_time



  b0 = res$summary$fixed_effects["(Intercept)", "mean"]

  ts =  res$random$time 
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns] 
  plot( mean ~ ID, ts, type="b", ylim=c(-2,2)+b0, lwd=1.5, xlab="year")
  lines( quant0.025 ~ ID, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ ID, ts, col="gray", lty="dashed")


  ts =  res$random$cyclic
  vns = c("mean", "quant0.025", "quant0.5", "quant0.975" ) 
  ts[, vns] = ts[, vns]
  plot( mean ~ID, ts, type="b", ylim=c(-1.5, 1.5)+b0, lwd=1.5, xlab="fractional year")
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
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature predictions", tmatch, umatch)  
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space", "combined" ), 
    sppoly=sppoly,
    breaks=seq(-5, 5, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title="Bottom temperature spatial effects (Celcius)"
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
    sppoly=sppoly,
    breaks=seq(-2, 2, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
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
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature spatiotemporal effects", tmatch, umatch)  
  )
  tmout

  # map all bottom temps:
  outputdir = file.path( gsub( ".rdata", "", carstm_filenames(p, returntype="carstm_modelled_fit") ), "figures" )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  graphics.off()
  # bbox = c(-71.5, 41, -52.5,  50.5 )


  background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 

  # slow due to use of webshot to save html to png (partial solution until tmap view mode saves directly)
  for (y in res$time ){
    for ( u in res$cyclic ){
      fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
      fn = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
      carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
        breaks=seq( 1, 9), 
        sppoly=sppoly,
        palette="-RdYlBu",
        title=fn_root,  
        outfilename=fn,
        plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
        background = background,
        vwidth = 1600,
        vheight=1000,
        map_mode="view",
        tmap_zoom= c(map_centre, map_zoom)
      )
    }
  }
# end

 