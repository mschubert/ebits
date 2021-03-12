include ../Makefile

all: data/ReactomePathways.gmt

data/ReactomePathways.gmt:
	wget --directory=data \
		http://www.reactome.org/download/current/ReactomePathways.gmt.zip
	unzip -d $(dir $@) $@.zip
