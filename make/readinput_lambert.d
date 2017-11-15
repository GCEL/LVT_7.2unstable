readinput_lambert.o readinput_lambert.d : readinput_lambert.F90
readinput_lambert.o : LVT_logMod.o
readinput_lambert.o : map_utils.o
readinput_lambert.o : LVT_coreMod.o
readinput_lambert.o : LVT_domainMod.o
