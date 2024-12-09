[www.metabrew.com](https://www.metabrew.com/)

Github builds the site with the jekyll image, then copies artifacts to nginx image and
publishes to GHCR.

The resulting nginx-based image runs behind traefik on my server.