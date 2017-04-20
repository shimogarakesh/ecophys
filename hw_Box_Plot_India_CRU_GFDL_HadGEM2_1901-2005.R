library(ncdf)

obs <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/CRU/India_annual_precip_CRU_1901-2005.nc")
obs1 <- get.var.ncdf(obs)
ens <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/ENSEMBLE_GFDL_HadGEM2/PR/India_annual_precip_ENSEMBLE_GFDL_HadGEM2_1901-2005.nc")
ens1 <- get.var.ncdf(ens)
gfdl_cm3 <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/GFDL_CM3/India_annual_precip_GFDL-CM3_1901-2005.nc")
gfdl_cm31 <- get.var.ncdf(gfdl_cm3)
gfdl_esm2g <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/GFDL_ESM2G/India_annual_precip_GFDL-ESM2G_1901-2005.nc")
gfdl_esm2g1 <- get.var.ncdf(gfdl_esm2g)
gfdl_esm2m <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/GFDL_ESM2M/India_annual_precip_GFDL-ESM2M_1901-2005.nc")
gfdl_esm2m1 <- get.var.ncdf(gfdl_esm2m)
hadgem2_ao <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/HadGEM2_AO/India_annual_precip_HadGEM2-AO_1901-2005.nc")
hadgem2_ao1 <- get.var.ncdf(hadgem2_ao)
hadgem2_cc <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/HadGEM2_CC/India_annual_precip_HadGEM2-CC_1901-2005.nc")
hadgem2_cc1 <- get.var.ncdf(hadgem2_cc)
hadgem2_es <- open.ncdf("C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Data_Files_for_Taylor_Diagram_Box_Plot/HadGEM2_ES/India_annual_precip_HadGEM2-ES_1901-2005.nc")
hadgem2_es1 <- get.var.ncdf(hadgem2_es)


#Box Plot
pdf(file="C:/Work_Kanhu/NCMRWF_Work/CMIP5_Models_work/Box_Plot_CMIP5_Models/Box_Plot_India_CRU_GFDL_HadGEM2_1901-2005.pdf",paper="letter")
boxplot(obs1, ens1, gfdl_cm31, gfdl_esm2g1, gfdl_esm2m1, hadgem2_ao1, hadgem2_cc1, hadgem2_es1, main="Boxplot",  ylab ="Rainfall(mm/day)", xlab =" ",las= 2, cex.axis=0.7, col = c("red","sienna","palevioletred1","royalblue2","yellow","purple","pink","brown"), at = c(1,2,3,4,5,6,7,8), names = c("CRU","Ensemble","GFDL_CM3","GFDL_ESM2G","GFDL_ESM2M","HadGEM2_AO","HadGEM2_CC","HadGEM2_ES"))
dev.off()