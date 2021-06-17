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

Now if you start nginx with `sudo nginx`, you should see Hideout's frontend running at `localhost`. This is also a good time to make sure Hideout can be accessed by visiting your public IP in your browser. You will need to configure port-forwarding for port 80 and 443 on your router and firewall for the traffic to be forwarded.

Now we need to setup domain name, DNS, and HTTPS. I'm switching to my perspective here, because there are multiple ways to make it work, and I don't want to sound like a sales associate by saying things like "okay now you should get a domain from Njalla and buy Mullvad VPN".

I got my domain name at Njalla: https://njal.la/, a "privacy-aware domain service". For the domain's DNS, I added an A record, fill in its name with "www", and its content with my public IP. I gave the record a short TTL. I didn't continue until I tested to see that I can reach Hideout by visiting `http://www.hideout-demo.com`. Note that it only works over HTTP, not HTTPS, at this point.

The next step is to enable HTTPS. Unlike domain and VPN, an HTTPS certificate can be acquired freely with EFF's Certbot: https://certbot.eff.org/. The instruction there is pretty simple to follow. Certbot modified my `/etc/nginx/conf.d/hideout-demo.com.conf` to handle HTTPS traffic, and redirect HTTP traffic to HTTPS.

I decided to test if I can access Hideout over HTTPS, and if I'll be redirected when I attempt HTTP connection. I found the website timing out. After checking every corner, it turned out that on my firewall, I've only setup port-forwarding for port 80, but not port 443. After I forwarded port 443, the HTTPS connection and redirection worked immediately.

At this point, I've successfully hosted a working instance of Hideout on my laptop. But if I'm to send a Hideout link to others, I'd expose the public IP of my home to both the recipients, and the unprivate platform where I send the link. So I need to host Hideout behind a VPN. Fortunately, port-forwarding is supported by Mullvad VPN: https://mullvad.net. I installed its open-source app on my laptop, and followed Mullvad's port-forwarding guide: https://mullvad.net/en/help/port-forwarding-and-mullvad/. Overall, it was a rather simple process. I first updated the DNS of the domain to point to the "Out" IP of Mullvad, which can be seen by clicking the expand arrow on the app's home interface. The ports were next. A caveat is that Mullvad assigns me a random port number. Let's assume it's 50000 in this guide. I first disabled the port-forwarding on my firewall for port 80 and 443, as they are unnecessary now. I thought I need to enable port-forwarding for port 50000, but somehow it worked without me doing so. The new port did require me to change the `listen 443 ssl` in `hideout-demo.com.conf` to `listen 50000 ssl`. Below is the final config. Notice the commented block at the end too.
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
# Is not needed here, because port 80 won't be available
# Through Mullvad anyway.
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
At this point, I've successfully hosted Hideout on a computer I physically own, over HTTPS, behind a VPN. I can access it in browser at `https://www.hideout-demo.com:50000`. Note that no piece of the URL can be left out. I need to specify `https`, because `http` isn't available. I need to specify `www`, because the DNS only has one record, which translates IP for `www`. I can't add some DNS redirect, because DNS isn't aware of port numbers. Finally, I need to specify the port number `50000` as well.
