all: data src analysis

data:
	make -C data
 
src:
	make -C src

analysis:
	make -C src/analysis
	
