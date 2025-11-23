FROM erlang:26-alpine AS builder

WORKDIR /app
COPY . .

RUN rebar3 as prod release

FROM alpine:3.19

RUN apk add --no-cache openssl ncurses

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/hsn_app .

EXPOSE 8080

CMD ["bin/my_app", "foreground"]