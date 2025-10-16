# Script wrapper to reduce need for documentation and avoid user errors:
# - podman mounts need absolute paths
# - we need to pass hostname to the container
#
# Paths are set to file paths in container ZIP package
#
MYDIR=`pwd`
podman run -v "$MYDIR"/config:/app/config -v "$MYDIR"/output:/app/output -v "$MYDIR"/data:/app/data --hostname localhost -it soib "$@"
