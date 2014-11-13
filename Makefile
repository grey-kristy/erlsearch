
REBAR = rebar
NAME = erlsearch
NODE = erlsearch_test@127.0.0.1
REL_DIR = $(CURDIR)/rel
REL_BUILD_DIR = $(CURDIR)/rel_build

TARGET_DIR=/opt/$(NAME)
TARGET_USER=$(NAME)

all: compile

compile: deps
	$(REBAR) compile

dc: 
	$(REBAR) compile skip_deps=true

deps:
	$(REBAR) get-deps
	
rel:
	$(REBAR) compile generate

start:
	erl -config priv/$(NAME).config -pa deps/*/ebin ebin -s $(NAME) -s reloader

test: 
	$(REBAR) compile skip_deps=true
	erlc -o test test/*.erl	
	./control test
	- rm $(CURDIR)/test/*.beam

clean:
	- rm $(CURDIR)/erl_crash.dump
	$(REBAR) skip_deps=true clean	

release: compile
	cd $(REL_DIR) && rm -rf $(NAME) && ../$(REBAR) generate
	
install: 
	mkdir -p $(TARGET_DIR)
	rsync -ra $(REL_DIR)/$(NAME)/* $(TARGET_DIR)/. && chown -R $(TARGET_USER):$(TARGET_USER) $(TARGET_DIR)
	ln -sfv $(TARGET_DIR)/platbox-wg-cpa /etc/init.d/	

.PHONY: compile deps clean test
