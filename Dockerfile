# parent image
FROM ubuntu:20.04

# Install any needed packages
RUN apt-get update 
RUN apt-get upgrade -y
RUN apt-get install -y cmake git g++ gfortran doxygen bash rsync curl mpich

