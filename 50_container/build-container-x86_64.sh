podman build --layers --squash-all -t soib -f 50_container/Dockerfile .
rm -rf 50_container/soib-container/x86_64
mkdir -p 50_container/soib-container/x86_64
podman save -o 50_container/soib-container/x86_64/soib.tar localhost/soib:latest
