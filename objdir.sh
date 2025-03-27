#!/bin/sh
# Create machine dependent object and binary directories for OASES
#
  if [ ! -d ./src/${1} ]; then
    mkdir ./src/${1}
  fi

  if [ ! -d ./src-3d ]; then
    mkdir ./src-3d
  fi

  if [ ! -d ./src-3d/${1} ]; then
    mkdir ./src-3d/${1}
  fi

  if [ ! -d ./src-rd ]; then
    mkdir ./src-rd
  fi

  if [ ! -d ./src-rd/${1} ]; then
    mkdir ./src-rd/${1}
  fi

  if [ ! -d ./src-mfp ]; then
    mkdir ./src-mfp
  fi

  if [ ! -d ./src-mfp/${1} ]; then
    mkdir ./src-mfp/${1}
  fi

  if [ ! -d ./plot/${1} ]; then
    mkdir ./plot/${1}
  fi

  if [ ! -d ./contour/${1} ]; then
    mkdir ./contour/${1}
  fi

  if [ ! -d ./pulsplot/${1} ]; then
    mkdir ./pulsplot/${1}
  fi

  if [ ! -d ./mindis/${1} ]; then
    mkdir ./mindis/${1}
  fi

  if [ ! -d ./lib/${1} ]; then
    mkdir ./lib/${1}
  fi

  if [ ! -d ./bin/${1} ]; then
    mkdir ./bin/${1}
  fi
