readGLEAMObs.o readGLEAMObs.d : readGLEAMObs.F90
readGLEAMObs.o : LVT_coreMod.o
readGLEAMObs.o : LVT_timeMgrMod.o
readGLEAMObs.o : LVT_logMod.o
readGLEAMObs.o : LVT_histDataMod.o
readGLEAMObs.o : GLEAM_obsMod.o
