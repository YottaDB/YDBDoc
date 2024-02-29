.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2018-2023 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. # Portions Copyright (c) Fidelity National                    #
.. # Information Services, Inc. and/or its subsidiaries.         #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

.. index::
   Containerization and YottaDB

===========================================
Appendix D : Containerization and YottaDB
===========================================

.. contents::
   :depth: 2


--------------------------
YottaDB Docker Container
--------------------------

The YottaDB Docker container can be found `here <https://hub.docker.com/r/yottadb/yottadb/>`_.

+++++++++++++
Quick Start
+++++++++++++

~~~~~~~~~~~~~~~
Pre-requisites
~~~~~~~~~~~~~~~

A working `Docker <https://www.docker.com/community-edition>`_ installation on your platform of choice.

NOTE: You must have at least docker 17.05 as `multi-stage <https://docs.docker.com/v17.09/engine/userguide/eng-image/multistage-build/>`_ builds are used within the docker file.

~~~~~~~~~~~~~~~~~~~~
Docker Pull Command
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   docker pull yottadb/yottadb

~~~~~~~~~~~~~~~~~~
Image Information
~~~~~~~~~~~~~~~~~~

The docker image is built using the ydb script that gives the user some sane defaults to begin exploring YottaDB. This is not meant for production usage.

The commands below assume that you want to remove the docker container after running the command, which means that if you don't mount a volume that contains your database and routines they will be lost. If you want the container to persist, remove the --rm parameter from the docker command.

Volumes are also supported by mounting to the /data directory. If you want to mount the local directory ydb-data into the container to save your database and routines locally and use them in the container in the future, add the following command line parameter before the yottadb/yottadb argument:

.. code-block:: bash

   -v pwd/ydb-data:/data

This creates a ydb-data directory in your current working directory. This can be deleted after the container is shutdown/removed if you want to remove all data created in the YottaDB container (such as your database and routines).

.. note::
   You may need to enable Docker by giving it the appropriate permissions. An :code:`ls -la /var/run/docker.sock` will show you what group can access the socket. Make sure to add your user to that group. Otherwise, depending on the permissions of the docker socket, you may need to use :code:`sudo docker` instead of :code:`docker`.

~~~~~~~~~~~~~~~~~~~~~~~~~~
Running a pre-built image
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   docker run --rm -it yottadb/yottadb # you can add a specific version after a ":" if desired

~~~~~~~~~~~~~
Build Steps
~~~~~~~~~~~~~

1. Build the image

   .. code-block:: bash

      docker build -t yottadb/yottadb:latest .

2. Run the created image.

   .. code-block:: bash

      docker run --rm -it yottadb/yottadb:latest

---------------------------------------------------
Recommendations for running YottaDB in Kubernetes
---------------------------------------------------

* YottaDB related logs should persist after a pod stops or is evicted. These logs contain very important information that help in diagnosing the problem. Refer to `<https://kubernetes.io/docs/concepts/cluster-administration/logging/>`_ for information on logging in Kubernetes.

* Use YottaDB's :ref:`Instance Freeze Capability <instance-freeze>`. Without instance freeze, if journaling cannot be continued due to limited disk-space or any other reasons, it is turned off automatically but the database is allowed to continue updating. This is fine if the situation is fixed and journaling is turned back on, but if the pod is lost then data recovery is problematic.

* Use a `preStop hook <https://kubernetes.io/docs/tasks/configure-pod-container/attach-handler-lifecycle-event/>`_ to stop processes accessing YottaDB gracefully when a pod needs to be stopped.

* Improve security by running all YottaDB processes in a single pod/multiple container set-up, rather than multiple pod/single container. The security issues are summarized here: `<https://serverfault.com/questions/1054724/ipc-between-multiple-pods-on-same-kubernetes-node>`_.
