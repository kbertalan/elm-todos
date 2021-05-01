#!/usr/sbin/env bash

elm-spa watch &
npx elm-live .elm-spa/defaults/Main.elm -p 5000 --proxy-prefix=/api --proxy-host=http://localhost:3000 -u -d public -- --debug --output=public/dist/elm.js

