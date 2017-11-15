readSMAPsmobs.o readSMAPsmobs.d : readSMAPsmobs.F90
readSMAPsmobs.o : LVT_misc.h
readSMAPsmobs.o : LVT_coreMod.o
readSMAPsmobs.o : LVT_histDataMod.o
readSMAPsmobs.o : LVT_logMod.o
readSMAPsmobs.o : SMAP_smobsMod.o
