#!/bin/bash -ex
docker run --rm \
  -e JEKYLL_ENV=production \
  --volume="$PWD:/srv/jekyll:Z" \
  --publish 4000:4000 \
  jekyll/jekyll \
  jekyll build
