FROM rocker/r-devel

ARG PKG_VER=0.1.4

RUN Rdevel -e  'install.packages(c("cli", "knitr", "jsonlite"))'

WORKDIR /home/

COPY checkglobals_${PKG_VER}.tar.gz .
RUN Rdevel CMD INSTALL checkglobals_${PKG_VER}.tar.gz

CMD ["Rdevel"]