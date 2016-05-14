
all: vsearch/vsearch ../inst/vsearch

vsearch/vsearch:
	cd vsearch; \
	./autogen.sh; \
	./configure; \
	make; \
	cd ..; \
	echo “Make of vsearch DONE”

../inst/vsearch: vsearch/vsearch
	mv vsearch/bin ../inst
	echo “Copy of vsearch DONE”

.PHONY: clean
clean:
	rm -f vsearch/*.o
	rm -f ../inst/vsearch ../inst/
