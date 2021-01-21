

  require(aegis.temperature)

  year.assessment = 2020

  # construct basic parameter list defining the main characteristics of the study
  # and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
  p = temperature_parameters( project_class="carstm", yrs=1950:year.assessment )

    # !!! WARNING, this uses a lot of RAM !!! 400 + GB with 4 cpus on default settings (1950-2020).. reduce 
    # adjust based upon RAM requirements and ncores
     inla.setOption(num.threads=4  )  # note, you want 1 here unless you have a lot of RAM and swap 
     inla.setOption(blas.num.threads= 1 )

  if (0) { 
        p$fraction_todrop = 1/5 # aggressiveness of solution finding ( fraction of counts to drop each iteration)
        p$fraction_cv = 1.0  #sd/mean no.
        p$fraction_good_bad = 0.9  # accept up to 90% of AUs near the target nmin
        p$areal_units_constraint_nmin = 40  # length(p$yrs)
        p$nAU_min = 100
  }
  
  # to recreate the underlying data
  # xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)

  sppoly = areal_units( p=p , redo=TRUE, verbose=TRUE )  # this has already been done in aegis.polygons::01 polygons.R .. should nto have to redo
  plot( sppoly[ "AUID" ] ) 



  M = temperature_db( p=p, DS="aggregated_data", redo=TRUE )  # will redo if not found .. not used here but used for data matching/lookup in other aegis projects that use bathymetry
  M = temperature_db( p=p, DS="carstm_inputs", redo=TRUE )  # will redo if not found
  # to extract fits and predictions

  fit = carstm_model( p=p, M="temperature_db( p=p, DS='carstm_inputs' ) " )   # 79 configs @ 110s / config = 2.4 hrs, ~ 20 hrs total

  # extract results
  if (0) {
    # very large files .. slow 
    fit = carstm_model( p=p, DS="carstm_modelled_fit" )  # extract currently saved model fit
    plot(fit)
    plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
  }

  res = carstm_model( p=p, DS="carstm_modelled_summary"  ) # to load currently saved results
  res$summary$dic$dic
  res$summary$dic$p.eff
  res$dyear

  # maps of some of the results
  time_match = list(year="2019", dyear="0.85")
  time_match = list(year="2020", dyear="0.85")

  vn = paste(p$variabletomodel, "predicted", sep=".")
  carstm_plot( p=p, res=res, vn=vn, time_match=time_match, breaksat=seq(-2, 10, by=2), 
    main=paste("Bottom temperature", paste0(time_match, collapse="-") )  )

  vn = paste(p$variabletomodel, "random_sample_iid", sep=".")
  carstm_plot( p=p, res=res, vn=vn, time_match=time_match )

  vn = paste(p$variabletomodel, "random_auid_nonspatial", sep=".")
  carstm_plot( p=p, res=res, vn=vn, time_match=time_match )

  vn = paste(p$variabletomodel, "random_auid_spatial", sep=".")
  carstm_plot( p=p, res=res, vn=vn, time_match=time_match )




# end




  if (0) {


      p = temperature_parameters(
        DS="parameters",
        assessment.years=1950:year.assessment,
        carstm_model_label = "testing",
        # inputdata_spatial_discretization_planar_km = 1,
        # areal_units_proj4string_planar_km = projection_proj4string("utm20"), # set up default map projection
        # areal_units_constraint = "snowcrab",
        areal_units_type= "tesselation",
        areal_units_constraint_nmin = 25,  # n time slices req in each au
        areal_units_resolution_km = 1,  # starting resolution
        sa_threshold_km2 = 5
      )
        # modeldir = project.datadirectory("bio.snowcrab", "modelled", "testing" ),  ## <--- important: specify save location
        # boundingbox = list( xlim = c(-70.5, -56.5), ylim=c(39.5, 47.5)), # bounding box for plots using spplot
      xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)

      sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create
      
  }

  
