FROM r-base:3.3.2

ADD . /

RUN ["Rscript", "install_packages.R"]

ENTRYPOINT ["/run.sh"]
