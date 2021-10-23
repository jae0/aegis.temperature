

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
  p = temperature_parameters( project_class="carstm", yrs=1970:year.assessment, carstm_model_label="default", 
      theta = c(  -0.837, -0.266, -0.414, 4.493, -1.586, -0.454, 23.405, 0.786  ) ) 
 #      theta = c( -0.837, -0.270, -0.961, 2.389, -0.413, 4.492, -1.805, -0.454, 23.391, 0.789 ) ) 
 
 

  # used by bio.snowcrab  --- about 24 hrs ( ~12 hrs + ;  6+ for mapping )... 
  p = temperature_parameters( project_class="carstm", yrs=1999:year.assessment, carstm_model_label="1999_present", 
      theta = c( -0.556, -6.324,  -0.733, 5.260, -0.642, -0.438, 21.244, 0.621  ) )
       


  # !!! WARNING: this uses a lot of RAM  
  fit = carstm_model( 
    p=p, 
    data ='temperature_db( p=p, DS="carstm_inputs" )',  
    num.threads="6:2",  # adjust for your machine
    mc.cores=2,
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    control.inla = list( strategy='adaptive', int.strategy="eb" ),
    verbose=TRUE 
  )    

	#, List of hyperparameters: 
	# 	theta[0] = [Log precision for the Gaussian observations]
	# 	theta[1] = [Log precision for cyclic]
        # theta[2] = [Log precision for space]
        # theta[3] = [Logit phi for space]
		# theta[4] = [Log precision for inla.group(z, method = "quantile", n = 11)]
	# 	theta[5] = [Log precision for space_time]
	# 	theta[6] = [Logit phi for space_time]
	# 	theta[7] = [Group rho_intern for space_time]
 


## --- long run results: 1970-present

 



##############


## --- short run results: 1999- present
# Fitted model saved as: /home/jae/bio.data/aegis/temperature/modelled/1999_present/temperature|canada.east|tesselation|1|none|40|10|none.rdata|carstm_modelled_summary|t|inla|100|7.rdata

# Deviance Information Criterion (DIC) ...............: 546440.39
# Deviance Information Criterion (DIC, saturated) ....: 2500670.27
# Effective number of parameters .....................: 10878.08

# Watanabe-Akaike information criterion (WAIC) ...: 547416.00
# Effective number of parameters .................: 10607.07


# Fixed effects
#             mean       sd           quant0.025 quant0.5   quant0.975
# (Intercept) 6.16773144 0.0600921552 6.04969833 6.16761527 6.28568136


# Random effects:
#                                                       mean             sd   quant0.025     quant0.5  quant0.975
# SD the Gaussian observations                    1.31971928  0.00274093815    1.3138489   1.31993557  1.32452319
# SD cyclic                                       18.4943731    0.584222092   17.5445605   18.4209961  19.8042772
# SD time                                         23.5569114    0.271565676   23.0005998    23.567039  24.0645223
# SD space                                        1.43986716   0.0278023228   1.38457729    1.4401821  1.49367044
# SD inla.group(z, method = "quantile", n = 11)   1.49313565     0.26008593   1.09551099   1.45176975  2.10720371
# SD space_time                                   1.24155046   0.0132395874   1.21421637   1.24214444  1.26602426
# Rho for time                                  -0.999747217 8.99040293e-06 -0.999764035 -0.999747555 -0.99972883
# Phi for space                                  0.995140678  0.00147205219  0.991924236   0.99525121 0.997603221
# Phi for space_time                             0.999999813 4.50164302e-06  0.999961747  0.999997158 0.999998399
# GroupRho for space_time                        0.299742681   0.0167560123  0.266076008  0.300014107 0.331725306
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
  # bbox = c(-71.5, 41, -52.5,  50.5 )


  background = tmap::tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8) 

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
        background = background,
        vwidth = 1600,
        vheight=1000,
        map_mode="view",
        tmap_zoom= c(map_centre, map_zoom)
      )
    }
  }
# end

 