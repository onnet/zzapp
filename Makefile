ROOT = ../..
PROJECT = onbill

all: compile copy_templates copy_schemas

copy_templates:
	cp -f priv/templates/teletype/* ../teletype/priv/templates/

copy_schemas:
	cp -f priv/couchdb/schemas/* ../crossbar/priv/couchdb/schemas/

include $(ROOT)/make/kz.mk
