#!/bin/bash

socat -d -d TCP-L:8099,fork UNIX:/var/run/docker.sock
