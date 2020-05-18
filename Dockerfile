FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get -y upgrade && \
    apt-get install -y git curl libpq-dev pkg-config libncurses5-dev libpcre3-dev

RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /data

COPY . /data

RUN stack install --local-bin-path bin

FROM ubuntu:20.04

RUN apt-get update && apt-get install -y libpq5 libatomic1

COPY --from=0 /data/bin/* /usr/bin/
COPY config.sample.yaml /config.yaml

ENTRYPOINT ["/usr/bin/yuntan-user"]

CMD ["-c", "/config.yaml"]
