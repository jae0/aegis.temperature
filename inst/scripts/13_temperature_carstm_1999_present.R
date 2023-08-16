  # 2022-01-14 14:17:20


  require(aegis.temperature)

  year.assessment = 2022
  loadfunctions("aegis.temperature")
  # about 24 hrs for 1999:2021
  # used by bio.snowcrab  --- about 24 hrs ( ~8 hrs + ;  6+ for mapping )... 
  # the theta are starting points for the hyper params on link scale
  p = temperature_parameters( 
    project_class="carstm", 
    carstm_model_label="1999_present",
    yrs=1999:year.assessment 
  )
  
  if (model_is_simple_cyclic) {
    p$theta =  c(-0.576, 0.460, 0.599, 1.551, -1.55, 1.406, -3.401, -0.547, 12.828, 0.679 ) 
  }

  if (model_is_space-cyclic) {

  # maxld= -220690.650 fn=3450 theta= 0.031 0.081 1.373 1.052 -1.269 -0.628 2.308 1.694 -1.129 -1.644 0.475 -0.769 0.468 [9.02, 27.978]

    p$theta = c(0.031, 0.081, 1.373, 1.052, -1.269, -0.628, 2.308, 1.694, -1.129, -1.644, 0.475, -0.769, 0.468) 

 		# theta[0] = [Log precision for the Gaussian observations]
		# theta[1] = [Log precision for time]
		# theta[2] = [Rho_intern for time]
		# theta[3] = [Log precision for cyclic]
		# theta[4] = [Log precision for space_cyclic]
		# theta[5] = [Logit phi for space_cyclic]
		# theta[6] = [Group rho_intern for space_cyclic]
		# theta[7] = [Log precision for space]
		# theta[8] = [Logit phi for space]
		# theta[9] = [Log precision for inla.group(z, method = "quantile", n = 11)]
		# theta[10] = [Log precision for space_time]
		# theta[11] = [Logit phi for space_time]
		# theta[12] = [Group rho_intern for space_time]
  }  
                
 


  # ------------------------------
  # prep data
    # to recreate the underlying data
    xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed

    xydata=temperature_db(p=p, DS="areal_units_input")  # redo if inpute data has changed
    xydata = xydata[ which(xydata$yr %in% p$yrs), ]
    sppoly = areal_units( p=p, xydata=xydata, spbuffer=10, n_iter_drop=3, redo=TRUE, verbose=TRUE )  # to force create

    plot( sppoly[ "AUID" ] ) 
    
    # or using carstm_map: 
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot

    sppoly = areal_units( p=p  )  # same
    
    M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE )  # must  redo if sppoly has changed or new data
    M = NULL
 

  # !!! WARNING: this uses a lot of RAM  
    res = NULL

    # M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly  ) 
    
    p$cyclic_levels = 1:10  # requires numeric

    res = carstm_model( 
      p=p, 
      data ='temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly)',  
      sppoly=sppoly,
      space_id = sppoly$AUID,
      time_id =  p$yrs,
      cyclic_id = p$cyclic_levels,
      nposteriors=1000,
      posterior_simulations_to_retain=c("predictions", "random_spatial"), 
      theta=p$theta,
      # if problems, try any of: 
      # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
      control.inla = list( strategy='adaptive', int.strategy="eb" ),
      # control.inla = list( strategy='laplace'  ),
      # redo_fit=FALSE,
      # debug = "random_spatiotemporal", 
      verbose=TRUE, 
      num.threads="5:2"  # adjust for your machine
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
  
    res$summary$fixed_effects["(Intercept)", "mean"]

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
    tmatch="2021"
    tmatch="2022"

    umatch="0.75"

    plt = carstm_map(  res=res, vn="predictions", tmatch=tmatch, umatch=umatch, 
      sppoly=sppoly,
      breaks=seq(-1, 9, by=2), 
      palette="-RdYlBu",
      plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
      tmap_zoom= c(map_centre, map_zoom),
      title=paste( "Bottom temperature predictions", tmatch, umatch)  
    )
    plt

    plt = carstm_map(  res=res, vn=c( "random", "space", "combined" ), 
      breaks=seq(-5, 5, by=1), 
      palette="-RdYlBu",
      plot_elements=c( "isobaths",  "compass", "scale_bar", "legend" ),
      tmap_zoom= c(map_centre, map_zoom),
      title="Bottom temperature spatial effects (Celsius)"
    )
    plt
 
    # map all bottom temps:
    outputdir = file.path(p$data_root, "maps", p$carstm_model_label )
    if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )
  
  
    graphics.off()
  
  
    additional_features = features_to_add( 
        p=p, 
        area_lines="cfa.regions",
        isobaths=c( 100, 200, 300, 400, 500  ), 
        coastline =  c("canada", "us"), 
        xlim=c(-80,-40), 
        ylim=c(38, 60) 
    )
  

    # e.g. management lines, etc
   
    fn_root = paste("Predicted_habitat_probability_persistent_spatial_effect", sep="_")
    outfilename = file.path( outputdir, paste(fn_root, "png", sep=".") )
    
    vn = c( "random", "space", "combined" ) 
    
    # toplot = carstm_results_unpack( res, vn )
    # brks = pretty(  quantile(toplot[,"mean"], probs=c(0,0.975), na.rm=TRUE )  )
    
    brks = pretty(c(-1, 1))
    plt = carstm_map(  res=res, vn=vn, 
      breaks = brks,
      palette="-RdYlBu",
      plot_elements=c(  "compass", "scale_bar", "legend" ),
      additional_features=additional_features,
      title= "Bottom temperature -- persistent spatial effect" ,
      outfilename=outfilename
    )  
    plt
 
    brks = pretty(c(1, 9))
    for (y in res$time_id ){
      for ( u in res$cyclic_id  ){
        fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
        outfilename = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
        plt = carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
          breaks=brks, 
          palette="-RdYlBu",
          plot_elements=c(  "compass", "scale_bar", "legend" ),
          additional_features=additional_features,
          title=fn_root,  
          outfilename=outfilename
        )
      }
    }
      
  # end

  