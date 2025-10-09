# Containers

Run one of build-containers-x86_64.sh or build-containers-aarch64.sh
from the root directory of the source code. E.g.

    $ sh 50_container/build-container-x86_64.sh

This will generate the container in
50_container/soib-container/x86_64/soib_container.tar

You'd typically need to run the aarch64 script separately (e.g. on a
Mac) and copy the resulting aarch64 directory into soib-container.
50_container/soib-container/x86_64/

Package soib-container directory next as a ZIP/tar, and distribute it
to your users. It is best to have both containers in one package
for ease of use.

You also need to create a job file that packages the data for the
container.  A job file may also be distributed with the container,
but that increases the size of the job file needlessly.
