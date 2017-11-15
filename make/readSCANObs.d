readSCANObs.o readSCANObs.d : readSCANObs.F90
readSCANObs.o : LVT_coreMod.o
readSCANObs.o : LVT_histDataMod.o
readSCANObs.o : LVT_timeMgrMod.o
readSCANObs.o : LVT_logMod.o
readSCANObs.o : SCAN_obsMod.o
readSCANObs.o : map_utils.o
