readGRDCobs.o readGRDCobs.d : readGRDCobs.F90
readGRDCobs.o : LVT_coreMod.o
readGRDCobs.o : LVT_histDataMod.o
readGRDCobs.o : LVT_timeMgrMod.o
readGRDCobs.o : LVT_logMod.o
readGRDCobs.o : GRDC_obsMod.o
readGRDCobs.o : map_utils.o
