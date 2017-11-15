readinput_latlon.o readinput_latlon.d : readinput_latlon.F90
readinput_latlon.o : LVT_coreMod.o
readinput_latlon.o : LVT_domainMod.o
readinput_latlon.o : LVT_logMod.o
readinput_latlon.o : map_utils.o
