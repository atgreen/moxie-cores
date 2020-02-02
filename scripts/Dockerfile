# This Dockerfile describes the moxie-cores test environment

FROM fedora:latest

ADD install.lisp /tmp/install.lisp

RUN dnf update -y \
    && dnf install -y \
           https://repos.moxielogic.org:7007/MoxieLogic/noarch/moxielogic-repo-latest.noarch.rpm \
    && dnf install -y verilator sbcl gcc-c++ \
           make moxielogic-moxie-elf-gcc wget \
	   iverilog \
    && dnf clean all \
    && wget https://github.com/atgreen/wrapilator/releases/download/v1.1.0/wrapilator \
    && mv wrapilator /usr/bin/ \
    && chmod +x /usr/bin/wrapilator \
    && wget http://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --non-interactive --load /tmp/install.lisp

CMD bash
