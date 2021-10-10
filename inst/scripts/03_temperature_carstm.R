

  require(aegis.temperature)

  year.assessment = 2021

  # construct basic parameter list defining the main characteristics of the study
  # and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
  # p = temperature_parameters( project_class="carstm", yrs=1950:year.assessment )
  
  # about 24 hrs for 1999:2021

  p = temperature_parameters( project_class="carstm", yrs=1950:year.assessment )

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
 
        sppoly = areal_units( p=p , redo=TRUE, verbose=TRUE )  # same
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
    
    gc()

    p = temperature_parameters( project_class="carstm", yrs=1969:year.assessment )
 
    M = temperature_db( p=p, DS="carstm_inputs"  )
    M = M[ which(M$yr %in% p$yrs), ]

  # !!! WARNING: this uses a lot of RAM  
  fit = carstm_model( 
    p=p, 
    data =M, 
    redo_fit = TRUE, 
    num.threads="4:2",
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    control.inla = list( strategy='adaptive' ),
    control.mode = list(theta = c(  -0.851, -5.656, -0.417, 4.720, -1.672, -0.459, 12.849, 0.766 ), restart=TRUE),  # to start optim from a solution close to the final in 2021 ... 
    verbose=TRUE 
  )    


    # in full AR1 model for time:
    
  	# theta[0] = [Log precision for the Gaussian observations]
		# theta[1] = [Log precision for cyclic]
    # drop these for factorial (yr_factor):
        # theta[2] = [Log precision for time] -8.986, 
        # theta[3] = [Rho_intern for time]  -7.043,
		# theta[4] = [Log precision for space]
		# theta[5] = [Logit phi for space]
		# theta[6] = [Log precision for inla.group(z, method = "quantile", n = 11)]
		# theta[7] = [Log precision for space_time]
		# theta[8] = [Logit phi for space_time]
		# theta[9] = [Group rho_intern for space_time]

# NOTE::: when using an AR1 on time, the cor= -1 ... might as well drop it from the model and use a factorial representation (see below):

#                                                            mean    sd 0.025quant 0.5quant 0.975quant   mode
# Precision for the Gaussian observations                   0.610 0.002      0.606    0.610      0.615  0.610
# Precision for cyclic                                     16.265 0.660     15.136   16.212     18.057 16.040
# Precision for time                                        0.002 0.000      0.002    0.002      0.003  0.003
# Rho for time                                             -0.999 0.000     -0.999   -0.999     -0.998 -0.999
# Precision for space                                       0.512 0.020      0.475    0.512      0.552  0.511
# Phi for space                                             0.986 0.003      0.981    0.986      0.991  0.985
# Precision for inla.group(z, method = "quantile", n = 11)  0.494 0.438      0.129    0.362      1.642  0.229
# Precision for space_time                                  0.671 0.014      0.645    0.670      0.698  0.669
# Phi for space_time                                          NaN   NaN      0.000    0.000        NaN    NaN
# GroupRho for space_time                                   0.252 0.017      0.217    0.252      0.285  0.252

# Deviance Information Criterion (DIC) ...............: 534970.61
# Deviance Information Criterion (DIC, saturated) ....: 2462069.13
# Effective number of parameters .....................: 11002.04

# Watanabe-Akaike information criterion (WAIC) ...: 535971.76
# Effective number of parameters .................: 10718.72

# Marginal log-Likelihood:  -231972.49 
# Posterior summaries for the linear predictor and the fitted values are computed
# (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')

#    --- NOTE: parameter estimates are on link scale and not user scale

# Computing summaries and computing from posterior simulations (can be longer than model fitting depending upon no of posterior sims: 'nposteriors' ) ...
# Extracting parameter summaries from marginals

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
  tmatch="1950"
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


   

