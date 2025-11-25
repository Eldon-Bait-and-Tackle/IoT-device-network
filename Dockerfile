FROM erlang:26-alpine AS builder

WORKDIR /app

RUN apk add --no-cache build-base git

COPY . .


RUN rebar3 as prod release



FROM alpine:3.19

RUN apk add --no-cache openssl ncurses libstdc++ libgcc

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/hsn_app .

EXPOSE 8082

CMD ["bin/hsn_app", "foreground"]