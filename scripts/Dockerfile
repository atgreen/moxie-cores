# This Dockerfile describes the moxie-cores test environment

FROM fedora:latest

RUN dnf install -y http://repos.moxielogic.org:7007/MoxieLogic/noarch/moxielogic-repo-latest.noarch.rpm \
    && dnf install -y verilator sbcl gcc-c++ make moxielogic-moxie-elf-gcc wget \
    && dnf clean all

RUN wget https://github.com/atgreen/wrapilator/releases/download/v1.0.0/wrapilator \
    && mv wrapilator /usr/bin/ \
    && chmod +x /usr/bin/wrapilator
RUN wget http://beta.quicklisp.org/quicklisp.lisp
ADD install.lisp /tmp/install.lisp
RUN sbcl --non-interactive --load /tmp/install.lisp

CMD bash
