readJULESobs.o readJULESobs.d : readJULESobs.F90
readJULESobs.o : LVT_coreMod.o
readJULESobs.o : LVT_timeMgrMod.o
readJULESobs.o : LVT_logMod.o
readJULESobs.o : LVT_histDataMod.o
readJULESobs.o : JULES_obsMod.o
readJULESobs.o : map_utils.o
