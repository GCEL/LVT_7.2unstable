readALEXIObs.o readALEXIObs.d : readALEXIObs.F90
readALEXIObs.o : LVT_misc.h
readALEXIObs.o : LVT_coreMod.o
readALEXIObs.o : LVT_timeMgrMod.o
readALEXIObs.o : LVT_logMod.o
readALEXIObs.o : LVT_histDataMod.o
readALEXIObs.o : ALEXI_obsMod.o
