FROM erlang:21.2.1-alpine
EXPOSE 8080
RUN apk add make g++
COPY . /app
WORKDIR /app
RUN rebar3 compile
ENTRYPOINT ["/usr/local/bin/rebar3"]