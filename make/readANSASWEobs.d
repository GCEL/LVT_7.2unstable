readANSASWEobs.o readANSASWEobs.d : readANSASWEobs.F90
readANSASWEobs.o : LVT_misc.h
readANSASWEobs.o : LVT_coreMod.o
readANSASWEobs.o : LVT_histDataMod.o
readANSASWEobs.o : LVT_logMod.o
readANSASWEobs.o : LVT_timeMgrMod.o
readANSASWEobs.o : ANSASWE_obsMod.o
