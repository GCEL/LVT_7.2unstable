readNatSFObs.o readNatSFObs.d : readNatSFObs.F90
readNatSFObs.o : LVT_coreMod.o
readNatSFObs.o : LVT_histDataMod.o
readNatSFObs.o : LVT_timeMgrMod.o
readNatSFObs.o : LVT_logMod.o
readNatSFObs.o : NatSF_obsMod.o
readNatSFObs.o : map_utils.o
