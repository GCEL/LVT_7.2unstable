readUWETObs.o readUWETObs.d : readUWETObs.F90
readUWETObs.o : LVT_coreMod.o
readUWETObs.o : LVT_logMod.o
readUWETObs.o : LVT_histDataMod.o
readUWETObs.o : UWET_obsMod.o
