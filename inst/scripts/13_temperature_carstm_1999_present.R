  # 2022-01-14 14:17:20


  require(aegis.temperature)

  year.assessment = 2022

  # about 24 hrs for 1999:2021
  # used by bio.snowcrab  --- about 24 hrs ( ~8 hrs + ;  6+ for mapping )... 
  # the theta are starting points for the hyper params on link scale
  p = temperature_parameters( 
    project_class="carstm", 
    carstm_model_label="1999_present",
    yrs=1999:year.assessment, 
    theta = c(-0.717, 0.460, 0.599, 1.557, -1.480, 1.406, -3.401, -0.547, 12.828, 0.679 ) 
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


  # ------------------------------
  # prep data
    # to recreate the underlying data
    xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed
    xydata = xydata[ which(xydata$yr %in% p$yrs), ]
    sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create

    plot( sppoly[ "AUID" ] ) 
    
    # or using carstm_map: 
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
    carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot

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
      num.threads="6:2",  # adjust for your machine
      mc.cores=2,
      # if problems, try any of: 
      # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
      # control.inla = list( strategy='adaptive', int.strategy="eb" ),
      control.inla = list( strategy='laplace'  ),
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
    # Deviance Information Criterion (DIC) ...............: 568920.50
    # Deviance Information Criterion (DIC, saturated) ....: 2524870.13
    # Effective number of parameters .....................: 7724.92

    # Watanabe-Akaike information criterion (WAIC) ...: 569130.99
    # Effective number of parameters .................: 7224.05

    # Marginal log-Likelihood:  -278942.24 
    # Posterior summaries for the linear predictor and the fitted values are computed
    # (Posterior marginals needs also 'control.compute=list(return.marginals.predictor=TRUE)')


    # $fixed_effects
    #                mean       sd quant0.025 quant0.5 quant0.975   parameter
    # (Intercept) 7.19164 0.216609     6.7663  7.19127    7.61622 (Intercept)

    # $random_effects
    #                                                   mean         sd quant0.025 quant0.5 quant0.975
    # SD the Gaussian observations                  1.430173 0.00214346  1.4255766 1.430346   1.433921
    # SD time                                       0.726388 0.14938807  0.4436396 0.726288   1.018488
    # SD cyclic                                     0.508445 0.14043420  0.3026282 0.483691   0.848791
    # SD space                                      2.032687 0.08870948  1.8376116 2.042843   2.176527
    # SD inla.group(z, method = "quantile", n = 11) 5.292217 1.31654681  3.0246229 5.197588   8.141208
    # SD space_time                                 1.318224 0.01600585  1.2887432 1.317409   1.351453
    # Rho for time                                  0.544143 0.25703429  0.0448915 0.564011   0.944560
    # GroupRho for space_time                       0.328919 0.01641543  0.2973550 0.328572   0.361639
    # Phi for space                                 0.764311 0.05451491  0.6316819 0.775127   0.839297
    # Phi for space_time                            0.996552 0.03835902  0.9995463 0.999756   0.999965
 
#  2022:
#    DIC:
# 	Mean of Deviance ................. 545755
# 	Deviance at Mean ................. 528685
# 	Effective number of parameters ... 17070.2
# 	DIC .............................. 562825
# DIC (Saturated):
# 	Mean of Deviance ................. 163551
# 	Deviance at Mean ................. 146643
# 	Effective number of parameters ... 17070.2
# 	DIC .............................. 180621
# Marginal likelihood: Integration -266213.760 Gaussian-approx -266204.821
# Compute the marginal for each of the 10 hyperparameters
#   Fixed effects:
#             mean    sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) 7.18 0.214      6.748    7.179      7.617 7.177   0

# Random effects:
#   Name	  Model
#     time AR1 model
#    cyclic RW2 model
#    space BYM2 model
#    inla.group(z, method = "quantile", n = 11) RW2 model
#    space_time BYM2 model

# Model hyperparameters:
#                                                           mean    sd 0.025quant
# Precision for the Gaussian observations                  0.609 0.000      0.608
# Precision for time                                       0.763 0.079      0.610
# Rho for time                                             0.038 0.055     -0.092
# Precision for cyclic                                     6.730 2.083      3.239
# Precision for space                                      0.098 0.000      0.095
# Phi for space                                            0.875 0.002      0.828
# Precision for inla.group(z, method = "quantile", n = 11) 0.002 0.000      0.001
# Precision for space_time                                 0.697 0.008      0.662
# Phi for space_time                                       0.734 0.002      0.696
# GroupRho for space_time                                  0.383 0.011      0.333
 
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
    tmatch="2021"
    tmatch="2022"

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
      title="Bottom temperature spatial effects (Celsius)"
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


    tmatch="2021"
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

  