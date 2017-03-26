FROM eshamster/cl-base:2.2

RUN apk add --no-cache --virtual=for-prepare git && \
    cd ${HOME}/.roswell/local-projects && \
    git clone https://github.com/eshamster/cl-shigi-simulator.git && \
    cd cl-shigi-simulator && \
    ros run -e '(ql:update-all-dists :prompt nil)' -q && \
    ros install qlot && \
    qlot install && \
    apk del for-prepare

RUN ros run -e '(ql:register-local-projects)' -e '(ql:quickload :cl-shigi-simulator)' -q

RUN ros install clack

ENV VIRTUAL_PORT 8080
COPY run_app.sh /root
CMD ["/root/run_app.sh"]
