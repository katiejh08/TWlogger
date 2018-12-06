##########################################
####         Calculate ODBA          #####
##########################################

# Calculate ODBA (Need to read about Wilson method, filter pass, and n)
e <- odba(A = AtCal, sampling_rate = Atstruct$sampling_rate, method = 'wilson', n = 1)
ba <- list(odba = e)
plott(ba, Atstruct$sampling_rate)


##########################################
####         Calculate VEDBA         #####
##########################################