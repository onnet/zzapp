ROOT = ../..
PROJECT = onbill
KZ_VERSION = $(shell grep vsn src/onbill.app.src | awk -F\" '{print $$2}')

all: compile copy_templates

copy_templates:
	cp -f priv/templates/teletype/* ../teletype/priv/templates/

include $(ROOT)/make/kz.mk
