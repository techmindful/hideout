Hideout can be hosted on a linux system with AMD64 CPU. Ideally I'd like to host it on a Raspberry Pi, because it's very affordable. But Haskell didn't play well with the ARM CPU on my Pi 3. So currently I've only successfully hosted Hideout on a spare laptop. I'll try Pi 4 when I get one. Rented VPS works too, but it's not ideal for Hideout. See section titled "Hideout is designed to be self-hosted" in [README](https://github.com/techmindful/hideout#hideout-is-designed-to-be-self-hosted).

Hideout is tested to work on Debian. Testing on other distros is very welcomed.

---

This guide assumes:
* Hideout will be hosted at `/home/user/hideout-1.0.0`. Your version number may be different.
* Domain name will be `hideout-demo.com`.

To host Hideout, first download the latest release from the release page: https://github.com/techmindful/hideout/releases

Unzip it to `/home/user/hideout-1.0.0`.

We need nginx for the server. Get it here: https://nginx.org/en/docs/install.html

We need to config nginx. You can skip this part if you are familiar with nginx. I'm no expert. I left nginx's default config at `/etc/nginx/nginx.conf` untouched:
```

user  nginx;
worker_processes  auto;

error_log  /var/log/nginx/error.log notice;
pid        /var/run/nginx.pid;


events {
    worker_connections  1024;
}


http {
    include       /etc/nginx/mime.types;
    default_type  application/octet-stream;

    log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
                      '$status $body_bytes_sent "$http_referer" '
                      '"$http_user_agent" "$http_x_forwarded_for"';

    access_log  /var/log/nginx/access.log  main;

    sendfile        on;
    #tcp_nopush     on;

    keepalive_timeout  65;

    #gzip  on;

    include /etc/nginx/conf.d/hideout-demo.com.conf;
}
```
Notice the last line `include /etc/nginx/conf.d/hideout-demo.com.conf;`. It brings in the site-specific config at `/etc/nginx/conf.d/hideout-demo.com.conf`. So let's create a config for Hideout at `/etc/nginx/conf.d/hideout-demo.com.conf`:
```
server {

  server_name www.hideout-demo.com;

  root /home/user/hideout-1.0.0/;

  location / {
    try_files $uri $uri/ /index.html;
  }

  location ~ /api {
    proxy_pass http://localhost:9000;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
  }

  listen 80;
}
```
Change your `server_name` and `root` based on your domain name, and file location.

Now if you start nginx with `sudo nginx`, you should see Hideout running at `localhost`.

Now is the time to setup domain name, DNS, and HTTPS. I'm going to switch to my perspective here, because there are multiple ways to make it work, and I don't want to sound like a sales associate by saying things like "okay now you should get a domain from Njalla and buy Mullvad VPN".

In my case, I'm hosting Hideout behind Mullvad VPN: https://mullvad.net. With its (open-source) app, port-forwarding through VPN becomes possible. It's an easy process, and I don't need to setup port-forwarding elsewhere. If you are hosting Hideout without a VPN, you probably need to setup port-forwarding on your router and firewall. A caveat is that Mullvad assigns me a random port number. Let's assume it's 50000 in this guide. This requires me to change the `listen 80` in `hideout-demo.com.conf` to `listen 50000`.

I get my domain name at Njalla: https://njal.la/, a "privacy-aware domain service". For the domain's DNS, I add an A record, fill in its name with "www", and its content with the "Out" IP shown on my Mullvad app. I give the record a short TTL. I don't continue until I test to see that I can reach Hideout by visiting `http://www.hideout-demo.com:50000`.

The next step is to enable HTTPS. Unlike domain and VPN, an HTTPS certificate can be acquired freely with EFF's Certbot: https://certbot.eff.org/. The instruction there is pretty simple to follow. I let Certbot modify my nginx config, and the final `hideout-demo.com.conf` looks like this:
```
server {

  server_name www.hideout-demo.com;

  root /home/user/hideout-1.0.0/;

  location / {
    try_files $uri $uri/ /index.html;
  }

  location ~ /api {
    proxy_pass http://localhost:9000;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
  }

  # SSL cert
  listen 50000 ssl;
  ssl_certificate /etc/letsencrypt/live/www.hideout-demo.com/fullchain.pem;
  ssl_certificate_key /etc/letsencrypt/live/www.hideout-demo.com/privkey.pem;
  include /etc/letsencrypt/options-ssl-nginx.conf;
  ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem;

}

# The traditional redirection from HTTP to HTTPS,
# Is not needed here. Because user only has 1 option for the port,
# If the server is behind Mullvad VPN.
#server {
#
#  server_name hideout-demo.com
#
#  if ($host = www.hideout-demo.com) {
#    return 301 https://$host$request_uri;
#  }
#
#  listen 80;
#
#  return 404;
#
#}
```
I modified it further after Certbot's operation. See the commented out block at the end. Make sure that the port number being listened on is 50000, with this line `listen 50000 ssl;`.

At this point, I've successfully hosted Hideout on a computer I physically own, behind VPN, over HTTPS. I can access it in browser at `https://www.hideout-demo.com:50000`. Note that no piece of the URL can be left out. I need to specify all of `https`, `www`, and port number `50000`.
