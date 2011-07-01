
clean : 
	rm -f *.Rout

all :
	R CMD BATCH --no-save testread.R &
	R CMD BATCH --no-save testReadPoints.R &
	R CMD BATCH --no-save testwrite.R &
	R CMD BATCH --no-save crash.R & 
