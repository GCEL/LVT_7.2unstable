readNLDAS2data.o readNLDAS2data.d : readNLDAS2data.F90
readNLDAS2data.o : LVT_coreMod.o
readNLDAS2data.o : LVT_logMod.o
readNLDAS2data.o : LVT_timeMgrMod.o
readNLDAS2data.o : LVT_histDataMod.o
readNLDAS2data.o : NLDAS2_dataMod.o
