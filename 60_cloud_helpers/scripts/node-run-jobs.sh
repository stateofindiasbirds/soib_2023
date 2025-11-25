#!/bin/sh
# Run one or more jobs on this node.
# Shutdown and deallocate this node when all
# those finish
#
# Jobs are expected in
#   /shared/config/<hostname>/*.R
# Output of running the jobs is logged into
#   /shared/logs/<hostname>/<rscript>-log
#

set -e

if [ -f /tmp/job-is-running ] ; then
    echo "Already running"
    exit 0
fi

# This will get cleaned up on reboot or done
touch /tmp/job-is-running

cd /shared/config/`hostname`
for f in *.R; do
    podman run \
      -v /shared/config:/app/config \
      -v /shared/output:/app/output \
      -v /shared/data:/app/data \
      --hostname `hostname` \
      -it soib $f 2>&1 > /shared/logs/`hostname`/$f-log
done

rm /tmp/job-is-running

echo "Shutting down and deallocating `hostname`"
# Shutdown and deallocate this node in one step
# PS: Hopefully the login won't fail!
az login --identity && az vm deallocate -g SoIBAnalysis -n `hostname`
