readsimGRACEObs.o readsimGRACEObs.d : readsimGRACEObs.F90
readsimGRACEObs.o : LVT_coreMod.o
readsimGRACEObs.o : LVT_logMod.o
readsimGRACEObs.o : LVT_histDataMod.o
readsimGRACEObs.o : LVT_timeMgrMod.o
readsimGRACEObs.o : simGRACE_obsMod.o
