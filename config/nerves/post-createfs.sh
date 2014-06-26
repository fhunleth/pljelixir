#!/bin/sh

TARGETDIR=$1

PROJECT_ROOT=$TARGETDIR/../../../..
FWUP_CONFIG=$PROJECT_ROOT/config/nerves/fwup.conf
BASE_FW_NAME=nerves-rpi-base

# Run the common post-image processing for nerves
$PROJECT_ROOT/_nerves/board/nerves-common/post-createfs.sh $TARGETDIR $FWUP_CONFIG $BASE_FW_NAME

