# this function was previously named LP0012_intersection_functions
# it was previously called from LP0012_rohe_intersections_rev.Rmd
# It has been renamed and adapted for EANZ on 28/6/22


#---
# Control Function ----

#---


intersection_tasks = function(tasklist, TA_ix_to_proc){
  
  
  for (iTA in 1:length(TA_ix_to_proc)){
    
    
    
    # iTA = 2
    
    # gc()
    
    
    
    
    if(!exists("TA")){TA = ea_load("TA")}
    TA_names = TA %>% pull(TA2019_V_1) %>% unique()
    curr_TA_ix = TA_ix_to_proc[iTA]
    TA_name_curr = TA_names[curr_TA_ix]
    
    
    TA_curr = TA[TA$TA2019_V_1 == TA_name_curr,]
    # mapview(TA_curr)
    
    print("****************")
    print(paste("Performing Intersection for", TA_name_curr," : TA #", curr_TA_ix,"....", iTA,"out of a total", length(TA_ix_to_proc),"to run"))
    print("****************")
    
    
    
    
    
    TA_folder_curr = paste0(TA_parent_folder, TA_name_curr,'/')
    TA_plotdir = paste0(plotdir, TA_name_curr,"/")
    
    # if (!dir.exists(TA_folder_curr)){dir.create(TA_folder_curr, recursive = T)}
    # if (!dir.exists(TA_plotdir)){dir.create(TA_plotdir, recursive = T)}
    
    
    #task 00: Write the TA Polygon
    if (0 %in% tasklist){TA_ix_task00(TA_curr, TA_folder_curr)}
    
    #task 01: TAxCL - intersect TA with the CL Layer 
    if (1 %in% tasklist){TAxCL = TA_ix_task01(TA_curr, TA_folder_curr)}
    
    #task 02: TAxPA - intersect TA with the PA Layer
    if (2 %in% tasklist){TAxPA = TA_ix_task02(TA_curr, TA_folder_curr)}
    
    #task 03: TAxCL - intersect TA with the properties with no title in the PLP Layer
    if (3 %in% tasklist){TAxPLP_CL = TA_ix_task03(TA_curr, TA_folder_curr)}
    
    #task 04: TAxCL - Find the properties that are owned by Crown Entities that have been listed in the CRoSL layer
    if (4 %in% tasklist){TAxPO_CL = TA_ix_task04(TA_curr, TA_folder_curr)}
    
    #task 05: TAxCL - Subtract the TAxCL from the TA
    if (5 %in% tasklist){TA_MINUS_CL_01 = TA_ix_task05(TA_curr, TA_folder_curr)}
    
    #task 06: TAxCL - Subtract the TAxPA from the TA_MINUS_CL_01
    if (6 %in% tasklist){TA_MINUS_CL_02 = TA_ix_task06(TA_curr, TA_folder_curr)}
    
    #task 07: TAxCL - Subtract the TAxPLP_CL from the TA_MINUS_CL_02
    if (7 %in% tasklist){TA_MINUS_CL_03 = TA_ix_task07(TA_curr, TA_folder_curr)}
    
    #task 08: TAxCL - Subtract the TAxPO_CL from the TA_MINUS_CL_03
    if (8 %in% tasklist){TA_MINUS_CL_04 = TA_ix_task08(TA_curr, TA_folder_curr)}
    
    #task 09: TA_NCLxPLP - intersect TA with the CL Layer 
    if (9 %in% tasklist){TA_NCLxPLP = TA_ix_task09(TA_curr, TA_folder_curr, PLP_gt_1ha = PLP_gt_1ha, use_intersects = F, save_ix_only = F)}
    
    #task 10: TA_NCLxML_gt_1ha 
    if (10 %in% tasklist){TA_NCLxML_gt_1ha = TA_ix_task10(TA_curr, TA_folder_curr, simplify_opt = F, TOL = 20, use_intersects = T)}
    
    #task 11: TA_NCLxLUM 
    if (11 %in% tasklist){TA_NCLxLUM = TA_ix_task11(TA_curr, TA_folder_curr, simplify_opt = T, TOL = 20)}
    
    #task 12: TA_NCLxAB_gt_1ha 
    if (12 %in% tasklist){TA_NCLxAB_gt_1ha = TA_ix_task12(TA_curr, TA_folder_curr, simplify_opt = T, TOL = 20)}
    
    
    
    
    #task 13: TA_NCLxGT 
    if (13 %in% tasklist){TA_NCLxGT = TA_ix_task13(TA_curr, TA_folder_curr)}
    
    #task 14: TA_NCLxLUMxML_gt_1ha 
    if (14 %in% tasklist){TA_NCLxLUMxML_gt_1ha = TA_ix_task14(TA_curr, TA_folder_curr)}
    
    #task 15: TA_NCLxPLP_gt_1haxGT 
    if (15 %in% tasklist){TA_NCLxPLP_gt_1haxGT = TA_ix_task15(TA_curr, TA_folder_curr, simplify_opt = T, TOL = 5)}
    
    #task 16: TA_NCLxPLP_gt_1haxML
    if (16 %in% tasklist){TA_NCLxPLP_gt_1haxML = TA_ix_task16(TA_curr, TA_folder_curr)}
    
    #task 17: TA_NLCxPLPxMLxLUM
    if (17 %in% tasklist){NCLxPLPxMLxLUM = TA_ix_task17(TA_curr, TA_folder_curr)}
    
    #task 18: TA_NLCxPLPxGTxLUM
    if (18 %in% tasklist){
      TA_NLCxPLPxGTxLUM = TA_ix_task18(TA_curr, TA_folder_curr)
    }
    
    #task 19: TAxLUM
    if (19 %in% tasklist){
      TAxLUM = TA_ix_task19(TA_curr, TA_folder_curr)
    }
    
    #task 21: TAxABxML
    if (21 %in% tasklist){
      TAxLUM = TA_ix_task21(TA_curr, TA_folder_curr)
    }
    
    #task 22: TAxABxGT
    if (22 %in% tasklist){
      TAxLUM = TA_ix_task22(TA_curr, TA_folder_curr)
    }
    
    #task 23: TAxPLPxFC
    if (23 %in% tasklist){
      TAxPLPxFC = TA_ix_task23(TA_curr, TA_folder_curr)
    }
    
    #task 24: TAxPLPxFCxLUM
    if (24 %in% tasklist){
      TAxPLPxFCxLUM = TA_ix_task24(TA_curr, TA_folder_curr)
    }
    
    #task 25: TA_NCLxAB_LP0012xML
    if (25 %in% tasklist){
      TAxPLPxFC = TA_ix_task23(TA_curr, TA_folder_curr)
    }
    
    #task 27: TAPLPxAB
    if (27 %in% tasklist){
      TAxPLPxAB = TA_ix_task27(TA_curr, TA_folder_curr)
    }
    
    #task 28: PLPxTA
    if (28 %in% tasklist){
      PLPxTA = TA_ix_task28(TA_curr, TA_folder_curr)
    }
    
    #task 29: LUCxTA
    if (29 %in% tasklist){
      LUCxTA = TA_ix_task29(TA_curr, TA_folder_curr)
    }
    
    #task 30: LUCxPLPxTA
    if (30 %in% tasklist){
      LUCxPLPxTA = TA_ix_task30(TA_curr, TA_folder_curr)
    }
    
    #task 31: LUMxTA
    if (31 %in% tasklist){
      LUMxTA = TA_ix_task31(TA_curr, TA_folder_curr)
    }
    
    #task 32: LUMxPLPxTA
    if (32 %in% tasklist){
      LUMxPLPxTA = TA_ix_task32(TA_curr, TA_folder_curr)
    }
    # 
    # 
    # #task 19: TA_NLCxLUMxML_gt_1ha
    # if (19 %in% tasklist){TA_NLCxLUMxML_gt_1ha = TA_ix_task19(TA_curr, TA_folder_curr, simplify_opt = F, TOL = 5)}
    # 
    # #task 19: TA_NLCxPLPxGTxLUM
    # if (18 %in% tasklist){TA_NLCxPLPxGTxLUM = TA_ix_task18(TA_curr, TA_folder_curr, simplify_opt = 5, TOL = 5)}
    # 
    # 
    
    
    
    
  }  # end of loop
  
  # copy TA_AREA_TABLE table to a csv file
  
  # TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  # TA_AREA_TABLE_CSV_ffn = paste0(TA_folder_curr, "TA_AREA_TABLE.csv")
  # write_csv(TA_AREA_TABLE, TA_AREA_TABLE_CSV_ffn)
  
}






TA_ix_task00 = function(TA_poly_curr, TA_folder_curr){
  
  
  TA_curr_ffn = paste0(TA_folder_curr,"TA.gpkg")
  st_write(TA_poly_curr, TA_curr_ffn, append = F)
  
  
  
}

# ---
# Task 1. intersect TA with the CL Layer ----
# ---


TA_ix_task01 = function(TA_poly_curr, TA_folder_curr){
  
  if (!exists("CL")){CL = ea_load("CL")}
  
  TAxCL = ea_intersection(P1 =CL ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'TAxCL', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  
  # 
  # if (!is.null(TAxCL)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TAArea_CL[curr_TA_ix] = calc_tot_area(TAxCL)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  # }
  
  # mapview(TAxCL)
  # mapview(TA_poly_curr)
  
  # mapview(TAxCL)
  
  
}


# ---
# Task 2. intersect TA with the PA Layer -----
# ---

TA_ix_task02 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  TAxPA = ea_intersection(P1 =PA ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'TAxPA', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  #   
  #   if (!is.null(TAxPA)){      #write area data
  #     TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #     TA_AREA_TABLE$TAArea_PA[curr_TA_ix] = calc_tot_area(TAxPA)
  #     saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)}
}




#---
# Task 3. intersect TA with the properties with no title in the PLP Layer ----
#---

TA_ix_task03 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  PLP_CL = ea_load("PLP_CL")
  PLP_CL_gt_1ha = PLP_CL %>% filter(PLP_AREA > 1.)
  
  
  TAxPLP_CL = ea_intersection(P1 =PLP_CL_gt_1ha ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'TAxPLP_CL', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  # 
  # if (!is.null(TAxPLP_CL)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TAArea_PLP_CL[curr_TA_ix] = calc_tot_area(TAxPLP_CL)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  # }
}


#---
# Task 4. Find the properties that are owned by Crown Entities that have been listed in the CRoSL layer ----
#---

TA_ix_task04 = function(TA_poly_curr, TA_folder_curr){
  
  
  PO_CL_ffn = paste0(datadir, "PO_CL.gpkg")
  PO_CL = st_read(PO_CL_ffn)
  
  
  TAxPO_CL = ea_intersection(P1 =PO_CL ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'TAxPO_CL', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  
  # SF = st_read(paste0(TA_folder_curr, "TAxPO_CL.gpkg"))
  
  
  # if (!is.null(TAxPO_CL)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TAArea_PO_CL[curr_TA_ix] = calc_tot_area(TAxPO_CL)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  #   
  #   # mapview(TAxPO_CL)
  # }
  
}

#---
# Task 5. TA_MINUS_CL_01 (TA - TAxCL) ----
# Subtract the TAxCL from the TA
#---

TA_ix_task05 = function(TA_poly_curr, TA_folder_curr){
  
  TAxCL_ffn = paste0(TA_folder_curr, "TAxCL.gpkg")
  TAxCL = st_read(TAxCL_ffn)
  
  if(!nrow(TAxCL)==0){
    TA_MINUS_CL_01 = getLayerDiff(TA_poly_curr,TAxCL) %>% validate_sh()
  }else{
    TA_MINUS_CL_01 = TA_poly_curr
  }
  
  TA_MINUS_CL_01_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_01.gpkg")
  st_write(TA_MINUS_CL_01, TA_MINUS_CL_01_ffn,append = F)
  # 
  # if (!is.null(TA_MINUS_CL_01)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TA_MINUS_CL_01_area[curr_TA_ix] = calc_tot_area(TA_MINUS_CL_01)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  # } 
}

#---
# Task 6. TA_MINUS_CL_02 (TA_MINUS_CL_01 - TAxPA) ----
# Subtract the TAxPA from the TA_MINUS_CL_01
#---

TA_ix_task06 = function(TA_poly_curr, TA_folder_curr){
  
  TAxPA_ffn = paste0(TA_folder_curr, "TAxPA.gpkg")
  TAxPA = st_read(TAxPA_ffn)
  
  TA_MINUS_CL_01_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_01.gpkg")
  TA_MINUS_CL_01 = st_read(TA_MINUS_CL_01_ffn) %>% st_simplify(.,preserveTopology = F, dTolerance = 10)
  
  if(!nrow(TAxPA)==0){
    TA_MINUS_CL_02 = getLayerDiff(TA_MINUS_CL_01,TAxPA) %>% validate_sh()
  }else{
    TA_MINUS_CL_02 = TA_poly_curr
  }
  
  TA_MINUS_CL_02_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_02.gpkg")
  st_write(TA_MINUS_CL_02, TA_MINUS_CL_02_ffn,append = F)
  # 
  # if (!is.null(TA_MINUS_CL_02)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TA_MINUS_CL_02_area[curr_TA_ix] = calc_tot_area(TA_MINUS_CL_02)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  # }
  
  print("---------------------------------------------------------------------")
  print(paste("====> finished task 6 for ", basename(TA_folder_curr)))
  print("---------------------+++ task complete +++---------------------------")
  
  
}

#---
# Task 7. TA_MINUS_CL_03 (TA_MINUS_CL_02 - TAxPLP_CL) ----
# Subtract the TAxPLP_CL from the TA_MINUS_CL_02
#---

TA_ix_task07 = function(TA_poly_curr, TA_folder_curr){
  
  TAxPLP_CL_ffn = paste0(TA_folder_curr, "TAxPLP_CL.gpkg")
  TAxPLP_CL = st_read(TAxPLP_CL_ffn)
  
  TA_MINUS_CL_02_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_02.gpkg")
  TA_MINUS_CL_02 = st_read(TA_MINUS_CL_02_ffn)
  
  
  if(!nrow(TAxPLP_CL)==0){
    TA_MINUS_CL_03 = getLayerDiff(TA_MINUS_CL_02,TAxPLP_CL) %>% validate_sh()
  }else{
    TA_MINUS_CL_03 = TA_MINUS_CL_02
  }
  
  TA_MINUS_CL_03_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_03.gpkg")
  st_write(TA_MINUS_CL_03, TA_MINUS_CL_03_ffn,append = F)
  
  # if (!is.null(TA_MINUS_CL_03)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TA_MINUS_CL_03_area[curr_TA_ix] = calc_tot_area(TA_MINUS_CL_03)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  # }
  # 
  print("---------------------------------------------------------------------")
  print(paste("====> finished task 7 for ", basename(TA_folder_curr)))
  print("---------------------+++ task complete +++---------------------------")
  
  
}

#---
# Task 8. TA_MINUS_CL_04 (TA_MINUS_CL_03 - TAxPO_CL) ----
# Subtract the TAxPO_CL from the TA_MINUS_CL_03
#---

TA_ix_task08 = function(TA_poly_curr, TA_folder_curr){
  
  TAxPO_CL_ffn = paste0(TA_folder_curr, "TAxPO_CL.gpkg")
  TAxPO_CL = st_read(TAxPO_CL_ffn)
  
  TA_MINUS_CL_03_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_03.gpkg")
  TA_MINUS_CL_03 = st_read(TA_MINUS_CL_03_ffn)
  
  
  if(!nrow(TAxPO_CL)==0){
    TA_MINUS_CL_04 = getLayerDiff(TA_MINUS_CL_03,TAxPO_CL) %>% validate_sh()
  }else{
    TA_MINUS_CL_04 = TA_MINUS_CL_03
  }
  
  TA_MINUS_CL_04_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_04.gpkg")
  st_write(TA_MINUS_CL_04, TA_MINUS_CL_04_ffn,append = F)
  # 
  # if (!is.null(TA_MINUS_CL_04)){      #write area data
  #   TA_AREA_TABLE = readRDS(TA_AREA_TABLE_ffn)
  #   TA_AREA_TABLE$TA_MINUS_CL_04_area[curr_TA_ix] = calc_tot_area(TA_MINUS_CL_04)
  #   TA_AREA_TABLE$TA_NCL_area[curr_TA_ix] = calc_tot_area(TA_MINUS_CL_04)
  #   saveRDS(TA_AREA_TABLE, TA_AREA_TABLE_ffn)
  # }
  
  print("---------------------------------------------------------------------")
  print(paste("====> finished task 8 for ", basename(TA_folder_curr)))
  print("---------------------+++ task complete +++---------------------------")
  
  
}

#---
# Task 9. TA_NCLxPLP ----
#---

TA_ix_task09 = function(TA_poly_curr, TA_folder_curr,  PLP_gt_1ha=NULL, use_intersects = F, save_ix_only = F){
  
  TA_NCL_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_04.gpkg")
  TA_NCL = st_read(TA_NCL_ffn)
  
  
  
  
  
  if (!exists("PLP_gt_1ha")){PLP_gt_1ha = ea_load("PLP_gt_1ha")}
  
  if (!use_intersects){
    
    
    
    TA_NCLxPLP_gt_1ha = ea_intersection(P1 =PLP_gt_1ha ,P2 = TA_NCL, PC =Current_PC, ffn_stub = 'TA_NCLxPLP_gt_1ha', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15, filetype = "SHP") 
    
  }else{
    
    
    tic()
    
    PLP_gt_1_ha_centroids = st_point_on_surface(PLP_gt_1ha)
    TA_NCLxPLP_gt_1ha_ix = which(st_intersects(PLP_gt_1_ha_centroids, TA_NCL) %>% lengths>0)
    
    if (!save_ix_only){
    
      TA_NCLxPLP_gt_1ha = PLP_gt_1ha[TA_NCLxPLP_gt_1ha_ix,]
      st_write(TA_NCLxPLP_gt_1ha, paste0(TA_folder_curr, "TA_NCLxPLP_gt_1ha", ".gpkg"), append = F)
    
    }else{
     
      
    
      dout = remove_accents(paste0(dirname(dirname(TA_folder_curr)),"/iwiPLPix/", basename(TA_folder_curr),"/"))
      if (!dir.exists(dout)){dir.create(dout, recursive = T)}
      
      fn_out = "TA_NCLxPLP_gt_1ha_ix.RDS"
      ffn_out =  paste0(dout, fn_out)
      file.rename(ffn_out, paste0(TA_folder_curr,fn_out ))
      # saveRDS(TA_NCLxPLP_gt_1ha_ix,ffn_out)
      saveRDS(TA_NCLxPLP_gt_1ha_ix, file = ffn_out)
      
    }
    # Error in gzfile(file, mode) : cannot open the connection
    # In addition: Warning message:
    #   In gzfile(file, mode) :
    # dir.exists(dn)
    #   
      
    toc()
    
    # mapview(TA_NCLxPLP_gt_1ha)
    
    
    
  }
  
  print("---------------------------------------------------------------------")
  print(paste("====> finished task 9 for ", basename(TA_folder_curr)))
  print("---------------------+++ task complete +++---------------------------")
  
  
}

#---
# 
# Task 10. TA_NCLxML_gt_1ha ----
#---

TA_ix_task10 = function(TA_poly_curr, TA_folder_curr, simplify_opt = F, TOL = 20, use_intersects = F){
  
  TA_NCL_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_04.gpkg")
  TA_NCL = st_read(TA_NCL_ffn)
  
  
  
  ML_gt_1ha_ffn = paste0(datadir, "ML_gt_1ha.gpkg")  

    if (!exists("ML_gt_1ha")){ML_gt_1ha = st_read(ML_gt_1ha_ffn)}
  
  if (!use_intersects){
    
    TA_NCLxML_gt_1ha = ea_intersection(P1 =ML_gt_1ha ,P2 = TA_NCL, PC =Current_PC, ffn_stub = 'TA_NCLxML_gt_1ha', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
    
  }else{
    
    ML_gt_1ha_centroids = st_point_on_surface(ML_gt_1ha)
    ML_gt_1ha_ffn_ix = which(st_intersects(ML_gt_1ha_centroids, TA_NCL) %>% lengths>0)
    TA_NCLxML_gt_1ha = ML_gt_1ha[ML_gt_1ha_ffn_ix,]
    
    
    
    st_write(TA_NCLxML_gt_1ha, paste0(TA_folder_curr, "TA_NCLxML_gt_1ha", ".gpkg"), append = F)
  }
  
  
  print("---------------------------------------------------------------------")
  print(paste("====> finished task 10 for ", basename(TA_folder_curr)))
  print("---------------------+++ task complete +++---------------------------")
  
  
}



#---
# Task 11. TA_NCLxLUM- here we are using LUM_LP0012 which has areas and a UID ----
#---

TA_ix_task11 = function(TA_poly_curr, TA_folder_curr, simplify_opt = F, TOL = 20){
  
  TA_NCL_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_04.gpkg")
  TA_NCL = st_read(TA_NCL_ffn)
  
  if (!exists("LUM_LP0012")){LUM_LP0012 = st_read(paste0(datadir,"LUM_LP0012.gpkg"))}
  
  TA_NCLxLUM = ea_intersection(P1 =LUM_LP0012 ,P2 = TA_NCL, PC =Current_PC, ffn_stub = 'TA_NCLxLUM', dn = TA_folder_curr, val = "both", 
                                   parallel = T, ncore = 15,simplify = simplify_opt, tol = TOL) 
  
  
  
}


#---
# Task 12. TA_NCLxAB_gt_1ha - here we are using AB_LP0012_gt_1ha which has areas and a UID and is filtered for properties > 1 ha ----
#---

TA_ix_task12 = function(TA_poly_curr, TA_folder_curr, simplify_opt = F, TOL = 20){
  
  TA_NCL_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_04.gpkg")
  TA_NCL = st_read(TA_NCL_ffn)
  
  if (!exists("AB_LP0012_gt_1ha")){AB_LP0012_gt_1ha = st_read(paste0(datadir,"AB_LP0012_gt_1ha.gpkg"))}
  
  # TA_NCLxAB_LP0012_gt_1ha = ea_intersection(P1 =AB_LP0012_gt_1ha ,P2 = TA_NCL, PC =Current_PC, ffn_stub = 'TA_NCLxAB_LP0012_gt_1ha', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15,
  #                                               simplify = simplify_opt, tol = TOL) 
  # 
  
  
  
  tic()
  #get the centrolids of the PLP
  AB_centroids_sf = st_point_on_surface(AB_LP0012_gt_1ha)
  
  #filter the original PLP file to only those centroids which lie within the GT area
  TA_NCLxAB_LP0012_gt_1ha=AB_LP0012_gt_1ha[which(st_intersects(AB_centroids_sf, TA_NCL) %>% lengths>0),]
  
  #save the filtered files
  TA_NCLxAB_LP0012_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxAB_LP0012_gt_1ha.gpkg")
  st_write(TA_NCLxAB_LP0012_gt_1ha, TA_NCLxAB_LP0012_gt_1ha_ffn, append = F)
  
  toc()
  
  
  
}


#---
# Task 13. Subtract the ML from the NML ----
#---

TA_ix_task13 = function(TA_poly_curr, TA_folder_curr){
  
  
  #load the TA_NCL (Non-crown land)
  TA_NCL_ffn = paste0(TA_folder_curr, "TA_MINUS_CL_04.gpkg")
  TA_NCL = st_read(TA_NCL_ffn)
  
  
  #load the ML layer
  if (!exists("ML")){ML = ea_load("ML")}
  
  #intersect ML with TA
  TA_NCLxML = ea_intersection(P1 =ML ,P2 = TA_NCL, PC =Current_PC, ffn_stub = 'TA_NCLxML', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  #get the difference between the TA and the ML to create GT land
  TA_NCLxGT = getLayerDiff(TA_NCL,TA_NCLxML) %>% validate_sh()
  TA_NCLxGT_ffn = paste0(TA_folder_curr, "TA_NCLxGT.gpkg")
  st_write(TA_NCLxGT, TA_NCLxGT_ffn,append = F)
  
  
  
}

#---
# Task 14. TA_NCLxLUMxML_gt_1ha  ----
#---

TA_ix_task14 = function(TA_poly_curr, TA_folder_curr){
  
  TA_NCLxLUM_ffn = paste0(TA_folder_curr, "TA_NCLxLUM.gpkg")
  TA_NCLxLUM = st_read(TA_NCLxLUM_ffn) %>% st_collection_extract("POLYGON")
  
  TA_NCLxML_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxML_gt_1ha.gpkg")
  TA_NCLxML_gt_1ha     = st_read(TA_NCLxML_gt_1ha_ffn) %>% st_collection_extract("POLYGON")
  
  
  NCLxMLxLUM = ea_intersection(P1 =TA_NCLxML_gt_1ha ,P2 = TA_NCLxLUM, PC =Current_PC, ffn_stub = 'NCLxMLxLUM', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15, filetype = "SHP") 
 
}



#---
# Task 15. TA_NCLxPLP_gt_1haxGT  ----
#---

# as of 23/5/22 we take a different approach here
# instead of using a spatial intersection on the polygons
# we find the centroids of each property over 1 ha then do a "st_intersects" on that



TA_ix_task15 = function(TA_poly_curr, TA_folder_curr, simplify_opt = F, TOL = 20){
  
  # get the Gen. title land (step 13)
  TA_NCLxGT_ffn = paste0(TA_folder_curr, "TA_NCLxGT.gpkg")
  TA_NCLxGT = st_read( TA_NCLxGT_ffn)
  
  
  
  # get the TAxPLP_gt_1ha (step 8)
  
  TA_NCLxPLP_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1ha.gpkg")
  TA_NCLxPLP_gt_1ha = st_read(TA_NCLxPLP_gt_1ha_ffn)
  
  tic()
  #get the centrolids of the PLP
  PLP_centroids_sf = st_point_on_surface(TA_NCLxPLP_gt_1ha)
  
  #filter the original PLP file to only those centroids which lie within the GT area
  TA_NCLxPLP_gt_1haxGT=TA_NCLxPLP_gt_1ha[which(st_intersects(PLP_centroids_sf, TA_NCLxGT) %>% lengths>0),]
  
  #save the filtered files
  TA_NCLxPLP_gt_1haxGT_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxGT.gpkg")
  st_write(TA_NCLxPLP_gt_1haxGT, TA_NCLxPLP_gt_1haxGT_ffn, append = F)
  
  toc()
  
  
  
  
  
  # old code commented out 23/5/22
  # TA_NCLxPLP_gt_1haxGT = ea_intersection(P1 =TA_NCLxPLP_gt_1ha ,P2 = TA_NCLxGT, PC =Current_PC, ffn_stub = 'TA_NCLxPLP_gt_1haxGT', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15, filetype = "SHP", simplify = simplify_opt, tol = TOL) 
  
  
  # mapview(TA_NCLxPLP_gt_1haxGT)
  
  
}



#---
# Task 16. TA_NCLxPLP_gt_1haxML ----
#---

TA_ix_task16 = function(TA_poly_curr, TA_folder_curr){
  
  # get the Maori Land Layer (step 13)
  TA_NCLxML_ffn = paste0(TA_folder_curr, "TA_NCLxML.gpkg")
  TA_NCLxML = st_read( TA_NCLxML_ffn)
  
  # get the TAxPLP_gt_1ha (step 8)
  
  TA_NCLxPLP_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1ha.gpkg")
  TA_NCLxPLP_gt_1ha = st_read(TA_NCLxPLP_gt_1ha_ffn)
  
  
  use_intersects = F
  
  if (!use_intersects){
  
  TA_NCLxPLP_gt_1haxML = ea_intersection(P1 =TA_NCLxPLP_gt_1ha ,P2 = TA_NCLxML, PC =Current_PC, ffn_stub = 'TA_NCLxPLP_gt_1haxML', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15, filetype = "SHP") 
  
  }else{
  tic()
  
  #combine the ML
  TA_NCLxML_cmb = st_combine(TA_NCLxML)
  
  
  #get the centrolids of the PLP
  PLP_centroids_sf = st_point_on_surface(TA_NCLxPLP_gt_1ha)
  
  #filter the original PLP file to only those centroids which lie within the combined ML area
  TA_NCLxPLP_gt_1haxML=TA_NCLxPLP_gt_1ha[which(st_intersects(PLP_centroids_sf, TA_NCLxML_cmb) %>% lengths>0),]
  
  #save the filtered files
  TA_NCLxPLP_gt_1haxML_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxML.gpkg")
  st_write(TA_NCLxPLP_gt_1haxML, TA_NCLxPLP_gt_1haxML_ffn, append = F)
  
  # mapview(TA_NCLxPLP_gt_1haxML)
  
  
  toc()
  }
  
  
  
  
  
  # mapview(TA_NCLxPLP_gt_1haxML)
  
  
}


#---
# Task 17.NCLxPLPxMLxLUM ----
#---

TA_ix_task17 = function(TA_poly_curr, TA_folder_curr){
  
  # get the Gen. title land (step 16)
  # TA_NCLxPLP_gt_1haxML_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxML.shp")
  TA_NCLxPLP_gt_1haxML_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxML.gpkg")
  TA_NCLxPLP_gt_1haxML = st_read( TA_NCLxPLP_gt_1haxML_ffn)
  
  # get the TA_NCLxLUM (step 11)
  
  TA_NCLxLUM_ffn = paste0(TA_folder_curr, "TA_NCLxLUM.gpkg")
  TA_NCLxLUM = st_read(TA_NCLxLUM_ffn)
  
  TA_NCLxPLPxMLxLUM = ea_intersection(P1 =TA_NCLxPLP_gt_1haxML ,P2 = TA_NCLxLUM, PC =Current_PC, ffn_stub = 'TA_NCLxPLPxMLxLUM', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15, filetype = "SHP") 
  
  
  # mapview(TA_NCLxPLP_gt_1haxML)
  
  
}




#---
# Task 18.NCLxPLPxMLxLUM ----
#---

TA_ix_task18 = function(TA_poly_curr, TA_folder_curr){
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # get the TA_NCLxPLP_gt_1haxGT land (step 15)
  # TA_NCLxPLP_gt_1haxGT_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxGT.shp")
  TA_NCLxPLP_gt_1haxGT_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxGT.gpkg")
  TA_NCLxPLP_gt_1haxGT = st_read( TA_NCLxPLP_gt_1haxGT_ffn)
  
  # get the TA_NCLxLUM_ffn (step XX)
  
  TA_NCLxLUM_ffn = paste0(TA_folder_curr, "TA_NCLxLUM.gpkg")
  TA_NCLxLUM = st_read(TA_NCLxLUM_ffn)
  
  
  
  TA_NCLxPLPxGTxLUM = ea_intersection(P1 =TA_NCLxPLP_gt_1haxGT ,P2 = TA_NCLxLUM, PC =Current_PC, ffn_stub = 'TA_NCLxPLPxGTxLUM', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15, filetype = "SHP") 
  
  
  # mapview(TA_NCLxPLP_gt_1haxML)
  
  
}



#---
# Task 19.TAxLUM ----
#---

TA_ix_task19 = function(TA_poly_curr, TA_folder_curr){
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # get the TA (step 0)
  TA_ffn = paste0(TA_folder_curr, "TA.gpkg")
  TA = st_read( TA_ffn)
  
  # get the LUM (step XX)
  if (!exists("LUM_LP0012")){LUM_LP0012 = st_read(paste0(datadir,"LUM_LP0012.gpkg"))}
  
  
  
  TAxLUM = ea_intersection(P1 =LUM_LP0012 ,P2 = TA, PC =Current_PC, ffn_stub = 'TAxLUM', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  
  # mapview(TA_NCLxPLP_gt_1haxML)
  
  
}




#---
# Task 21.TAxABxML ----
#---

TA_ix_task21 = function(TA_poly_curr, TA_folder_curr){
  
  
  # TA_folder_curr = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rev/TA_level_data_sets/Kurahaupo/"
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # get the TA_NCLxAB_LP0012_gt_1ha (step 12)
  TA_NCLxAB_LP0012_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxAB_LP0012_gt_1ha.gpkg")
  TA_NCLxAB_LP0012_gt_1ha = st_read( TA_NCLxAB_LP0012_gt_1ha_ffn)
  
  TA_NCLxAB_LP0012_gt_1ha
  
  
  
  # get the TA_NCLxML_gt_1ha (step 10)
  TA_NCLxML_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxML_gt_1ha.gpkg")
  TA_NCLxML_gt_1ha = st_read( TA_NCLxML_gt_1ha_ffn)
  
  
  TAxABxML = ea_intersection(P1 =TA_NCLxAB_LP0012_gt_1ha ,P2 = TA_NCLxML_gt_1ha, PC =Current_PC, ffn_stub = 'TAxABxML', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  
  # mapview(TAxABxML)
  
  
}




#---
# Task 22.TAxABxGT ----
#---

TA_ix_task22 = function(TA_poly_curr, TA_folder_curr){
  
  
  # TA_folder_curr = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rev/TA_level_data_sets/Kurahaupo/"
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # get the TA_NCLxAB_LP0012_gt_1ha (step 12)
  TA_NCLxAB_LP0012_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxAB_LP0012_gt_1ha.gpkg")
  TA_NCLxAB_LP0012_gt_1ha = st_read( TA_NCLxAB_LP0012_gt_1ha_ffn)
  
  # get the TA_NCLxPLP_gt_1haxGT land (step 15)
  TA_NCLxPLP_gt_1haxGT_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxGT.shp")
  TA_NCLxPLP_gt_1haxGT = st_read( TA_NCLxPLP_gt_1haxGT_ffn)
  
  
  TAxABxGT = ea_intersection(P1 =TA_NCLxAB_LP0012_gt_1ha ,P2 = TA_NCLxPLP_gt_1haxGT, PC =Current_PC, ffn_stub = 'TAxABxGT', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  
  # mapview(TAxABxGT)
  
  
}




#---
# Task 23.TAxPLPxFC ----
#---

TA_ix_task23 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # get the Forest Clearance Data
  FC = ea_load("FC")
  
  #get the TA_NCLxPLP_gt_1ha land (step 15)
  TA_NCLxPLP_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1ha.gpkg")
  TA_NCLxPLP_gt_1ha = st_read( TA_NCLxPLP_gt_1ha_ffn)
  
  
  TA_NCLxPLP_gt_1haxFC = ea_intersection(P1 =FC ,P2 = TA_NCLxPLP_gt_1ha, PC =Current_PC, ffn_stub = 'TA_NCLxPLP_gt_1haxFC', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15) 
  
  
  # mapview(TAxABxGT)
  
  
}


#---
# Task 24.TAxPLPxFCxLUM ----
#---

TA_ix_task24 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  
  #get the TA_NCLxPLP_gt_1haxFC (step 23)
  TA_NCLxPLP_gt_1haxFC_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxFC.gpkg")
  TA_NCLxPLP_gt_1haxFC = st_read( TA_NCLxPLP_gt_1haxFC_ffn)
  
  
  #get the TA_NCLxLUM (step 11)
  TA_NCLxLUM_ffn = paste0(TA_folder_curr, "TA_NCLxLUM.gpkg")
  TA_NCLxLUM = st_read( TA_NCLxLUM_ffn)
  
  
  TAxPLPxFCxLUM = ea_intersection(P1 =TA_NCLxPLP_gt_1haxFC ,P2 = TA_NCLxLUM, PC =Current_PC, ffn_stub = 'TAxPLPxFCxLUM', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15,filetype = "SHP") 
  
  
  # mapview(TAxABxGT)
  
  
}



#---
# Task 25.TAxPLPxFCxLUM ----
#---

TA_ix_task24 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  
  #get the TA_NCLxPLP_gt_1haxFC (step 23)
  TA_NCLxPLP_gt_1haxFC_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1haxFC.gpkg")
  TA_NCLxPLP_gt_1haxFC = st_read( TA_NCLxPLP_gt_1haxFC_ffn)
  
  
  #get the TA_NCLxLUM (step 11)
  TA_NCLxLUM_ffn = paste0(TA_folder_curr, "TA_NCLxLUM.gpkg")
  TA_NCLxLUM = st_read( TA_NCLxLUM_ffn)
  
  
  TAxPLPxFCxLUM = ea_intersection(P1 =TA_NCLxPLP_gt_1haxFC ,P2 = TA_NCLxLUM, PC =Current_PC, ffn_stub = 'TAxPLPxFCxLUM', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15,filetype = "SHP") 
  
  
  # mapview(TAxABxGT)
  
  
}




#---
# Task 27.TAxPLPxAB ----
#---

TA_ix_task27 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  
  #get the TA_NCLxPLP_gt_1ha (step 9)
  TA_NCLxPLP_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxPLP_gt_1ha.gpkg")
  TA_NCLxPLP_gt_1ha = st_read( TA_NCLxPLP_gt_1ha_ffn)
  
  
  #get the TA_NCLxAB_LP0012_gt_1ha_ffn (step 12)
  TA_NCLxAB_LP0012_gt_1ha_ffn = paste0(TA_folder_curr, "TA_NCLxAB_LP0012_gt_1ha.gpkg")
  TA_NCLxAB_LP0012_gt_1ha = st_read( TA_NCLxAB_LP0012_gt_1ha_ffn)
  
  
  TAxPLPxAB = ea_intersection(P1 =TA_NCLxPLP_gt_1ha ,P2 = TA_NCLxAB_LP0012_gt_1ha, PC =Current_PC, ffn_stub = 'TAxPLPxAB', dn = TA_folder_curr, val = "both", parallel = T, ncore = 15,filetype = "GPKG") 
  
  
  # mapview(TAxABxGT)
  
  
}








#---
# Task 28.PLPxTA ---- 
#---

TA_ix_task28 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # TA_poly_curr = TA_curr
  
  # if (!exists("PLP")){PLP = ea_load("PLP")}
  
  #get the indices of the PLP that lie inside the TA
  PLP_in_TA_ix = which(st_intersects(PLP_points, TA_poly_curr) %>% lengths>0)
  #reduce the PLP data set just to those that lie inside for faster processing
  PLP_sel  = PLP[PLP_in_TA_ix,]
 
  
  PLPxTA = ea_intersection(P1 =PLP_sel ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'PLPxTA', dn = TA_folder_curr, val = "None", parallel = T, ncore = 15,filetype = "GPKG") 
  
  
  # mapview(TAxABxGT)
  
  
}




#---
# Task 29.PLPxLUCxTA ---- 
#---

TA_ix_task29 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  LUCxTA =  ea_intersection(P1 =LUC ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'LUCxTA', dn = TA_folder_curr, val = "None", parallel = T, ncore = 15,filetype = "GPKG") 
  
  
  
  
  
}


#---
# Task 30.LUCxPLPxTA ---- 
#---

TA_ix_task30 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  
  LUCxTA_ffn = paste0(TA_folder_curr,"LUCxTA.gpkg")
  LUCxTA =  st_read(LUCxTA_ffn)
  
  PLPxTA_ffn = paste0(TA_folder_curr,"PLPxTA.gpkg")
  PLPxTA =  st_read(PLPxTA_ffn)
  
  LUCxPLPxTA =  ea_intersection(P1 =LUCxTA ,P2 = PLPxTA, PC =Current_PC, ffn_stub = 'LUCxPLPxTA', dn = TA_folder_curr, val = "None", parallel = T, ncore = 15,filetype = "GPKG") 
  
  
}


#---
# Task 31.LUMxTA ---- 
#---

TA_ix_task31 = function(TA_poly_curr, TA_folder_curr){
  
  
  #test code ------------------------------------
  testcode=F
  if (testcode){
  iTA = 2
  if(!exists("TA")){TA = ea_load("TA")}
  TA_names = TA %>% pull(TA2019_V_1) %>% unique()
  curr_TA_ix = TA_ix_to_proc[iTA]
  TA_name_curr = TA_names[curr_TA_ix]
  TA_poly_curr = TA[TA$TA2019_V_1 == TA_name_curr,]
  TA_folder_curr = paste0(TA_parent_folder, TA_name_curr,'/')
  TA_plotdir = paste0(plotdir, TA_name_curr,"/")
  }
  # mapview(TA_curr)
  #----------------------------------------------
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  # PLP_in_TA_ix = which(st_intersects(PLP_points, TA_poly_curr) %>% lengths>0)
  
  LUMxTA_ix = which(st_intersects(LUM,TA_poly_curr) %>% lengths>0)
  
  LUM_sel = LUM[LUMxTA_ix,] %>% dplyr::select(-SHAPE_Leng)
  
  mapview(LUM_sel)
  
  LUMxTA =  ea_intersection(P1 =LUM_sel ,P2 = TA_poly_curr, PC =Current_PC, ffn_stub = 'LUMxTA', dn = TA_folder_curr, val = "None", parallel = T, ncore = 15,filetype = "GPKG") 
  
  
}


#---
# Task 32.LUMxPLPxTA ---- 
#---

TA_ix_task32 = function(TA_poly_curr, TA_folder_curr){
  
  
  
  
  
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(paste("+++++      TA CURRENT FOLDER = ", TA_folder_curr))
  print("+++++++++++++++++++++++++++++++++++++++++++++++++++")
  
  LUMxTA_ffn = paste0(TA_folder_curr,"LUMxTA.gpkg")
  LUMxTA =  st_read(LUMxTA_ffn)
  
  PLPxTA_ffn = paste0(TA_folder_curr,"PLPxTA.gpkg")
  PLPxTA =  st_read(PLPxTA_ffn)
  
  LUMxPLPxTA =  ea_intersection(P1 =LUMxTA ,P2 = PLPxTA, PC =Current_PC, ffn_stub = 'LUMxTA', dn = TA_folder_curr, val = "None", parallel = T, ncore = 15,filetype = "GPKG") 
  
  
}


