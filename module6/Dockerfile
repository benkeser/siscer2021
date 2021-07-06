# Start by pulling ubuntu image
FROM rocker/rstudio:latest

# update libraries
RUN apt-get -y update

# non-interactive mode
ENV DEBIAN_FRONTEND=noninteractive

# install R libraries needed for analysis
# COPY R/r_package_installs.R /home/lib/r_package_installs.R
# RUN chmod +x /home/lib/r_package_installs.R && /home/lib/r_package_installs.R
# RUN rm /home/lib/r_package_installs.R
RUN Rscript -e 'install.packages("nloptr", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("xgboost", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("earth", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("glmnet", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("ranger", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("rpart", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("quadprog", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("ggplot2", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("SuperLearner", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("ltmle", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("drtmle", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("plotmo", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("plotrix", repos="https://cran.rstudio.com")'
RUN Rscript -e 'install.packages("TeachingDemos", repos="https://cran.rstudio.com")'


# make directories
# lib contains R source files
# dat contains data 
RUN mkdir /home/data /home/lib /home/Lab1 /home/Lab2 /home/Lab3

# add lab code 
COPY Lab1/* /home/lib/Lab1/
COPY Lab2/* /home/lib/Lab2/
COPY Lab3/* /home/lib/Lab3/

# copy over user info
COPY roster.csv /home/roster.csv
COPY make_users.sh /home/make_users.sh


# make users
RUN chmod +x /home/make_users.sh && /home/make_users.sh \
  && rm /home/roster.csv
