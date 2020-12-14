
#   !!! WARNING, this uses a lot of RAM !!! 96 GB


  year.assessment = 2019

  # construct basic parameter list defining the main characteristics of the study
  # and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
  p = aegis.temperature::temperature_parameters( project_class="carstm", yrs=1950:year.assessment )


  # to recreate the underlying data
  sppoly = areal_units( p=p, redo=TRUE )  # this has already been done in aegis.polygons::01 polygons.R .. should nto have to redo
  spplot( sppoly, "AUID", main="AUID", sp.layout=p$coastLayout )


  M = temperature_db( p=p, DS="aggregated_data", redo=TRUE )  # will redo if not found .. not used here but used for data matching/lookup in other aegis projects that use bathymetry
  M = temperature_db( p=p, DS="carstm_inputs", redo=TRUE )  # will redo if not found
  # to extract fits and predictions

  fit = carstm_model( p=p, M=M )

  # extract results
  fit = carstm_model( p=p, DS="carstm_modelled_fit" )  # extract currently saved model fit
  res = carstm_summary( p=p, operation="load"  ) # to load currently saved results

  plot(fit)
  plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
  s = summary(fit)
  s$dic$dic
  s$dic$p.eff

  # maps of some of the results
  vn = paste(p$variabletomodel, "predicted", sep=".")
  carstm_plot( p=p, res=res, vn=vn )

  vn = paste(p$variabletomodel, "random_sample_iid", sep=".")
  if (exists(vn, res)) carstm_plot( p=p, res=res, vn=vn, time_match=list(year="1950", dyear="0") )

  vn = paste(p$variabletomodel, "random_auid_nonspatial", sep=".")
  if (exists(vn, res)) {
    res_dim = dim( res[[vn]] )
    if (res_dim == 1 ) time_match = NULL
    if (res_dim == 2 ) time_match = list(year="2000")
    if (res_dim == 3 ) time_match = list(year="2000", dyear="0.8" )
    carstm_plot( p=p, res=res, vn=vn, time_match=time_match )
  }

  vn = paste(p$variabletomodel, "random_auid_spatial", sep=".")
  if (exists(vn, res)) {
    res_dim = dim( res[[vn]] )
    if (res_dim == 1 ) time_match = NULL
    if (res_dim == 2 ) time_match = list(year="2000")
    if (res_dim == 3 ) time_match = list(year="2000", dyear="0.8" )
    carstm_plot( p=p, res=res, vn=vn, time_match=time_match )
  }

}


# end