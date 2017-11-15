readAmerifluxObs.o readAmerifluxObs.d : readAmerifluxObs.F90
readAmerifluxObs.o : LVT_coreMod.o
readAmerifluxObs.o : LVT_histDataMod.o
readAmerifluxObs.o : LVT_timeMgrMod.o
readAmerifluxObs.o : LVT_logMod.o
readAmerifluxObs.o : Ameriflux_obsMod.o
readAmerifluxObs.o : map_utils.o
