readGlobSnowObs.o readGlobSnowObs.d : readGlobSnowObs.F90
readGlobSnowObs.o : LVT_coreMod.o
readGlobSnowObs.o : LVT_logMod.o
readGlobSnowObs.o : LVT_histDataMod.o
readGlobSnowObs.o : GlobSnow_obsMod.o
