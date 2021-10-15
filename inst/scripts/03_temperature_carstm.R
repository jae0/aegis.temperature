

  require(aegis.temperature)

  year.assessment = 2021


  # construct basic parameter list defining the main characteristics of the study
  p = temperature_parameters( project_class="carstm", yrs=1900:year.assessment )

    if (0) { 
        require(INLA)
        inla.setOption(num.threads=2  )  # note, you want 1 here unless you have a lot of RAM and swap 

        # to recreate the underlying data
        xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed
        # sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create

          p$fraction_cv = 1.0
          p$fraction_todrop = 1/5
#          p$areal_units_constraint_nmin = 30  # n time slices req in each au >> nyears as we resolve season
          p$areal_units_constraint_ntarget = 40  # n time slices req in each au
          p$areal_units_constraint_nmin = 10   # n time slices req in each au
 
        sppoly = areal_units( p=p , hull_alpha=15, redo=TRUE, verbose=TRUE )  # same
        plot( sppoly[ "AUID" ] ) 
        
        # or using carstm_map: 
        carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
        carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot


    }

    # update data ... must redo if input data has changes or new data

    (p$yrs) # check the years to ensure we are selecting the correct years 1950:present
    temperature_db ( DS="bottom.all.redo", p=p ) 

    o = temperature_db ( DS="aggregated_data", p=p, redo=TRUE )   
    o = NULL

    M = temperature_db( p=p, DS="carstm_inputs", redo=TRUE )  # must  redo if sppoly has changed or new data
    M = NULL
    


  # subset years ... earlier than 1970:present can take a lot RAM in posterior extraction ... reduce number of cores .. perhaps to 1 (of course this will be slower but alternative is to crash the system)
  # about 24 hrs for 1999:2021

  # choose one: 

  # used by aegis.survey (cod, wolfish, etc)
  p = temperature_parameters( project_class="carstm", yrs=1970:year.assessment, carstm_model_label="default", mc.cores=2, 
      theta = c(-0.699, -0.475, -1.191, 2.456, 1.657, 2.466, -0.666, -1.494, 23.031, -3.218 ) ) 
  

  # used by bio.snowcrab
  p = temperature_parameters( project_class="carstm", yrs=1999:year.assessment, carstm_model_label="1999_present", mc.cores=3, 
      theta = c( -0.552, -5.862, -8.714, -6.981, -0.662, 4.816, -1.024, -0.448, 20.803, 0.646 ) )

  


  # !!! WARNING: this uses a lot of RAM  
  fit = carstm_model( 
    p=p, 
    data ='temperature_db( p=p, DS="carstm_inputs" )', 
    num.threads="6:2",  # adjust for your machine
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    control.inla = list( strategy='adaptive' ),
    verbose=TRUE 
  )    

	#, List of hyperparameters: 
	# 	theta[0] = [Log precision for the Gaussian observations]
	# 	theta[1] = [Log precision for cyclic]
	# 	theta[2] = [Log precision for space]
	# 	theta[3] = [Logit phi for space]
	# 	theta[4] = [Log precision for inla.group(z, method = "quantile", n = 11)]
	# 	theta[5] = [Log precision for space_time]
	# 	theta[6] = [Logit phi for space_time]
	# 	theta[7] = [Group rho_intern for space_time]
 
# NOTE::: when using an AR1 on time, the cor= -1 ... might as well drop it from the model and use a factorial representation (see below):
 
# Deviance Information Criterion (DIC) ...............: 534970.61
# Deviance Information Criterion (DIC, saturated) ....: 2462069.13
# Effective number of parameters .....................: 11002.04

# Watanabe-Akaike information criterion (WAIC) ...: 535971.76
# Effective number of parameters .................: 10718.72

# Marginal log-Likelihood:  -231972.49 
# Posterior summaries for the linear predictor and the fitted values are computed
# (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

# 
# Fixed effects
#             mean      sd          quant0.025 quant0.5   quant0.975
# (Intercept) 6.1717286 0.111128617 5.95339243 6.17155215 6.38969323

# Model may be over parameterized. NAN and Inf values encountered. Try alt parameterizations or smaller number of n or masking negative values

# Random effects:
#                                                     mean            sd  quant0.025    quant0.5  quant0.975
# SD the Gaussian observations                  1.28016351 0.00239205016   1.2754022  1.28018691  1.28479192
# SD cyclic                                     0.24790958  0.0047944458 0.237208228 0.248511453 0.255537996
# SD time                                        20.587215    1.27526226  18.8691944  20.3331901   23.655753
# SD space                                      1.39783646  0.0265768005  1.34634171  1.39754159   1.4506838
# SD inla.group(z, method = "quantile", n = 11)  1.6908805   0.523304436 0.781357514  1.66265463  2.78140715
# SD space_time                                 1.22137344  0.0122219904  1.19683107  1.22160161  1.24478417
#    --- NOTE: 'SD *' are on link scale and not user scale


    # extract results
    if (0) {
      # very large files .. slow 
      fit = carstm_model( p=p, DS="carstm_modelled_fit" )  # extract currently saved model fit
      fit$summary$dic$dic
      fit$summary$dic$p.eff
    
      plot(fit)
      plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
    }


  res = carstm_model( p=p, DS="carstm_modelled_summary"  ) # to load currently saved results

  
  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7

  # maps of some of the results
  tmatch="1970"
  umatch="0.15"

  tmout = carstm_map(  res=res, vn="predictions", tmatch=tmatch, umatch=umatch, 
    breaks=seq(-1, 9, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature predictions", tmatch, umatch)  
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space", "combined" ), 
    breaks=seq(-5, 5, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title="Bottom temperature spatial effects (Celcius)"
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
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

  # slow due to use of webshot to save html to png (partial solution until tmap view mode saves directly)
  for (y in res$time ){
    for ( u in res$cyclic ){
      fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
      fn = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
      carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
        breaks=seq( 1, 9), 
        palette="-RdYlBu",
        title=fn_root,  
        outfilename=fn,
        plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
        vwidth = 1600,
        vheight=1000,
        tmap_zoom= c(map_centre, map_zoom)
      )
    }
  }
  
# end


   

