FROM eshamster/cl-base:2.1

RUN apk add --no-cache --virtual=for-prepare git && \
    cd ${HOME}/.roswell/local-projects && \
    git clone https://github.com/eshamster/cl-shigi-simulator.git && \
    ros install qlot && \
    cd cl-shigi-simulator && \
    qlot install && \
    apk del for-prepare

RUN ros run -e '(push #P"~/.roswell/local-projects/cl-shigi-simulator/quicklisp" ql:*local-project-directories*)' -e '(ql:quickload :cl-shigi-simulator)' -q

RUN ros install clack

ENV VIRTUAL_PORT 8080
COPY run_app.sh /root
CMD ["/root/run_app.sh"]
