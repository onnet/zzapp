ROOT = ../..
PROJECT = onbill

all: compile copy_templates check_mysql_deps

copy_templates:
	cp -f priv/templates/teletype/* ../teletype/priv/templates/

check_mysql_deps:
	if grep -q "mysql" $(ROOT)/make/deps.mk; then \
	   echo "mysql exists"; \
	else \
	  echo "" >> $(ROOT)/make/deps.mk; \
	  echo "DEPS += mysql mysql_poolboy" >> $(ROOT)/make/deps.mk; \
	  echo "dep_mysql = git https://github.com/mysql-otp/mysql-otp 1.3.0" >> $(ROOT)/make/deps.mk; \
	  echo "dep_mysql_poolboy = git https://github.com/mysql-otp/mysql-otp-poolboy 0.1.7" >> $(ROOT)/make/deps.mk; \
	  echo "" >> $(ROOT)/make/deps.mk; \
	fi

include $(ROOT)/make/kz.mk
