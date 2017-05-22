ROOT = ../..
PROJECT = onbill

all: compile copy_templates

copy_templates:
	cp -f priv/templates/teletype/* ../teletype/priv/templates/

include $(ROOT)/make/kz.mk
