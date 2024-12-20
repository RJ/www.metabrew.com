FROM jekyll/jekyll AS builder
WORKDIR /home/jekyll/app
RUN chmod 777 /home/jekyll/app
COPY . .
RUN touch Gemfile.lock && chmod go+rw Gemfile.lock
RUN mkdir -p _site && chmod go+rwx _site
ENV JEKYLL_ENV=production
RUN jekyll build

FROM nginx:latest
COPY --from=builder /home/jekyll/app/_site /usr/share/nginx/html
COPY --from=builder /home/jekyll/app/nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80

# Run nginx in foreground for docker:
CMD ["nginx", "-g", "daemon off;"]