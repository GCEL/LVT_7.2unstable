readCEOP.o readCEOP.d : readCEOP.F90
readCEOP.o : LVT_misc.h
readCEOP.o : LVT_coreMod.o
readCEOP.o : LVT_histDataMod.o
readCEOP.o : LVT_timeMgrMod.o
readCEOP.o : LVT_logMod.o
readCEOP.o : CEOP_obsMod.o
readCEOP.o : map_utils.o
