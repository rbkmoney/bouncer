REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := bouncer
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := ef20e2ec1cb1528e9214bdeb862b15478950d5cd

BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := aaa79c2d6b597f93f5f8b724eecfc31ec2e2a23b

CALL_ANYWHERE := \
	submodules \
	all compile xref lint format check_format dialyze cover release clean distclean

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

.PHONY: $(CALL_W_CONTAINER) all

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile: submodules
	$(REBAR) compile

xref:
	$(REBAR) xref

lint:
	$(REBAR) lint

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze:
	$(REBAR) dialyzer

release: submodules
	$(REBAR) as prod release

clean:
	$(REBAR) cover -r
	$(REBAR) clean

distclean:
	$(REBAR) clean
	rm -rf _build

cover:
	$(REBAR) cover

# CALL_W_CONTAINER
test: submodules
	$(REBAR) do eunit, ct
