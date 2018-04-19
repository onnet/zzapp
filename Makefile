ROOT = ../..
PROJECT = zzapp
KZ_VERSION = $(shell grep vsn src/zzapp.app.src | awk -F\" '{print $$2}')

all: compile copy_templates move_beams

copy_templates:
	cp -f priv/templates/teletype/* ../teletype/priv/templates/

move_beams:
	mv ebin/webhooks_*.beam ../webhooks/ebin/

include $(ROOT)/make/kz.mk
