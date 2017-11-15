readARSsmobs.o readARSsmobs.d : readARSsmobs.F90
readARSsmobs.o : LVT_coreMod.o
readARSsmobs.o : LVT_histDataMod.o
readARSsmobs.o : LVT_timeMgrMod.o
readARSsmobs.o : LVT_logMod.o
readARSsmobs.o : ARSsm_obsMod.o
