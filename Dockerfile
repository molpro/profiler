# parent image
FROM alpine:latest

# Install any needed packages
RUN apk update && apk add --no-cache \ 
cmake g++ gfortran git doxygen build-base binutils file util-linux bash rsync

RUN apk update && apk add --no-cache procps
