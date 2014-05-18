#!/bin/sh

TARGETDIR=$1

PROJECT_ROOT=$TARGETDIR/../../../..
FWTOOL_CONFIG=$PROJECT_ROOT/config/nerves/fwtool.config
BASE_FW_NAME=nerves-rpi-base

# Run the common post-image processing for nerves
$PROJECT_ROOT/_nerves/board/nerves-common/post-createfs.sh $TARGETDIR $FWTOOL_CONFIG $BASE_FW_NAME

