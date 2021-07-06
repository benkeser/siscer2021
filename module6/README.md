# SISCER 2020 Docker Container

**Authors:** [David Benkeser](https://www.github.com/benkeser/)

-----

## Description

This repo contains all code used to create the Docker image that was used 
to run the lab code during [Module 6 of UW SISCER 2020]
(https://si.biostat.washington.edu/suminst/archives/SISCER2021/CR2106). 

-----

## Usage

This GitHub repository contains the source code needed to build the
docker image that was used in class. The built image is also available on
[DockerHub](https://cloud.docker.com/u/dbenkeser/repository/docker/dbenkeser/siscer2020).
The code for the labs can be found in the `/R` directory.

When the docker image is built, a number of `R` packages are installed and 
user accounts are made for all individuals in `roster.csv` using `make_users.sh`.
The `roster.csv` is expected to have two columns, where the first column is the 
participant's last name, the second column is the first name. The script creates
users of the form `[first letter of first name][last name]`. For confidentiality, 
I've replace the true course roster with fake names (that result in users: `auser`,
`buser`, `cuser`, `duser`, and `euser`). 

To run the docker image on a local machine (i.e., to obtain the same R Studio
environment that was demonstrated in class), you need to [download docker](https://docs.docker.com/docker-for-windows/install/) to your local machine. 

From the command line you would execute the following command.

``` bash
docker run -e PASSWORD=sosecret! -p 8787:8787 -d dbenkeser/siscer:latest
```

Here the `-e` option is required to run the `rocker/rstudio` image that 
our Docker image is built on. The `-d` option runs in detached mode, which makes 
the container run in the background. The `-p` option opens port 87, so you 
can access the RStudio GUI, by opening a browser window and navigating to 
`localhost:8787`, where you will be 
prompted to login to R Studio. You can use the default user (`rstudio`) and the 
`PASSWORD` created above or using the credentials we made based on `roster.csv`. 

The container can also be run through a cloud service (e.g., an [AWS EC2 instance](https://aws.amazon.com/ec2/?hp=tile&so-exp=below), or a [Digital Ocean](https://www.digitalocean.com/) droplet). Almost all services have machine 
images that include docker. All you have to do is `ssh` into the cloud server and
execute the same command as above. Then users can log in via `http://ipaddress:8787`
where `ipaddress` is the ip address of the cloud server. 

## Issues

If you encounter any bugs or can't remember how to run the container you can
[file an issue](https://github.com/benkeser/siscer2020/issues).

-----

## License

© 2020- David Benkeser

The contents of this repository are distributed under the MIT license:

    The MIT License (MIT)
    
    Copyright (c) 2021- David C. Benkeser
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
