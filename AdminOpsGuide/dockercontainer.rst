
.. index::
   Docker Container

=====================================
Appendix F: YottaDB Docker Container
=====================================

The YottaDB Docker container can be found `here <https://hub.docker.com/r/yottadb/yottadb/>`_.

-----------------
Quick Start
-----------------

+++++++++++++++
Pre-requisites
+++++++++++++++

A working `Docker <https://www.docker.com/community-edition#/download>`_ installation on your platform of choice.

NOTE: You must have at least docker 17.05 as `multi-stage <https://docs.docker.com/v17.09/engine/userguide/eng-image/multistage-build/>`_ builds are used within the docker file.

++++++++++++++++++++
Docker Pull Command
++++++++++++++++++++

.. parsed-literal::
   docker pull yottadb/yottadb

++++++++++++++++++
Image Information
++++++++++++++++++

The docker image is built using the ydb script that gives the user some sane defaults to begin exploring YottaDB. This is not meant for production usage.

The commands below assume that you want to remove the docker container after running the command, which means that if you don't mount a volume that contains your database and routines they will be lost. If you want the container to persist, remove the --rm parameter from the docker command.

Volumes are also supported by mounting to the /data directory. If you want to mount the local directory ydb-data into the container to save your database and routines locally and use them in the container in the future, add the following command line parameter before the yottadb/yottadb argument:

.. parsed-literal::
   -v pwd/ydb-data:/data

This creates a ydb-data directory in your current working directory. This can be deleted after the container is shutdown/removed if you want to remove all data created in the YottaDB container (such as your database and routines).

.. note::
   You may need to enable Docker by giving it the appropriate permissions. An ls -la on /var/run/docker.sock will show you what group can access the socket. Make sure to add your user to that group. Otherwise, depending on the permissions of the docker socket, you may need to use ``sudo docker`` instead of ``docker``.

++++++++++++++++++++++++++
Running a pre-built image
++++++++++++++++++++++++++

.. parsed-literal::
   docker run --rm -it yottadb/yottadb # you can add a specific version after a ":" if desired


+++++++++++++++++++++
Build Steps
+++++++++++++++++++++

1. Build the image

   .. parsed-literal::
      docker build -t yottadb/yottadb:latest .

2. Run the created image.

   .. parsed-literal::
      docker run --rm -it yottadb/yottadb:latest


