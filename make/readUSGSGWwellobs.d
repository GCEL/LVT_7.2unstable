readUSGSGWwellobs.o readUSGSGWwellobs.d : readUSGSGWwellobs.F90
readUSGSGWwellobs.o : LVT_coreMod.o
readUSGSGWwellobs.o : LVT_histDataMod.o
readUSGSGWwellobs.o : LVT_logMod.o
readUSGSGWwellobs.o : LVT_timeMgrMod.o
readUSGSGWwellobs.o : USGSGWwell_obsMod.o
readUSGSGWwellobs.o : map_utils.o
readUSGSGWwellobs.o : LVT_String_Utility.o
