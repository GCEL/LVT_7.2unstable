readFMISWEobs.o readFMISWEobs.d : readFMISWEobs.F90
readFMISWEobs.o : LVT_coreMod.o
readFMISWEobs.o : LVT_histDataMod.o
readFMISWEobs.o : LVT_timeMgrMod.o
readFMISWEobs.o : LVT_logMod.o
readFMISWEobs.o : FMISWE_obsMod.o
