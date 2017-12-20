
.. index::
   IPC Resource Usage

================================================
Appendix A : YottaDB/GT.M's IPC Resource Usage
================================================

.. contents::
   :depth: 2

In YottaDB/GT.M's database engine, processes cooperate with one another to manage the database. To effect this cooperation, YottaDB/GT.M processes use UNIX Interprocess Communication (IPC) resources to coordinate and control access to the database and journal files, implement M LOCKs, and for database replication. This appendix helps you understand YottaDB/GT.M's IPC resource utilization.

This appendix includes two sections.

* The first section contains an exercise to identify "orphan" shared memory segments (those with no attached processes) in a multi-site replication configuration. Usually orphan segments appear due to certain abnormal terminations (for example, kill -KILL or a process crash situation) and create an out-of-design state for YottaDB/GT.M IPC resources where the last process may not be able to clean up the shared memory segment. If you know the IPC resources YottaDB/GT.M uses, then you can easily find the abnormal IPC resources when something does not seem right.

* The second section describes role of the gtmsecshr daemon process YottaDB/GT.M uses to manage M locks and clean up IPC resources. This section also includes guidelines to fine-tune gtmsecshr for smooth operation.

---------------------------------------
Examining YottaDB/GT.M's IPC Resources
---------------------------------------

YottaDB/GT.M uses UNIX IPC resources as follows:

* For each database region, YottaDB/GT.M uses a shared memory segment (allocated with shmat()) for control structures and to implement M Locks. For journaled databases, the journal buffers reside in that shared memory segment. With the BG database access method, global buffers for the database also reside there. Note that use of the online help system by a process opens a database file with the BG access method. The first process to open a database file creates and initializes the shared memory segment, and the last process to exit normally cleans up and deletes the shared memory segment. However, under certain abnormal terminations of the last process (for example, if it is terminated with a kill -KILL), that last process may not be able to clean up the shared memory segment, resulting in "orphan" shared memory segments (those with no attached processes).

* For database regions which use the MM access method, the file system manages an additional shared memory segment (allocated with mmap()) to memory map the database file. YottaDB/GT.M does not explicitly allocate this shared memory. Because UNIX allocates shared memory segment when YottaDB/GT.M opens a database file, and releases it when the process terminates, such shared memory segments allocated by mmap() are never orphaned.

* When replicating, YottaDB/GT.M implements the journal pool on the primary in a shared memory segment. On the secondary, YottaDB/GT.M uses a shared memory segment for the receive pool.

* YottaDB/GT.M operations such as creating a shared memory segment for a given database file should be performed only by one process even if more than one process opens the database file at the same time. YottaDB/GT.M uses sets of public UNIX semaphores to insure these operations are single-threaded. YottaDB/GT.M uses other sets of public semaphores to setup and tear down shared memory segments allocated with shmat().

* Public semaphore ids may be non-unique. Private semaphore ids are always unique for each database.

* The semaphore with keys starting 0x2b and 0x2c are startup and rundown semaphores. A YottaDB/GT.M process uses them only while attaching to or detaching from a database.

* The number of processes and the number of semaphore attached to an IPC resource may vary according to the state of your database. Some shared memory regions have 0 processes attached to them (the nattch column). If these correspond to YottaDB/GT.M database regions or to global directories, they are most likely from improper process termination of YottaDB/GT.M (YottaDB/GT.M processes show up as " mumps " in a ps command) and YottaDB/GT.M utility processes; source server, receiver server, or update processes (which appear as " mupip "); or other YottaDB/GT.M utilities (" mupip ", " dse ", or " lke ").

* An instance has one journal pool, and, if a replicating instance, one receiver pool too. Note that you might run multiple instances on the same computer system.

* For simple YottaDB/GT.M operation (that is, no multisite replication), there is no journal pool or receive pool.

The following exercise demonstrates how YottaDB/GT.M utilizes IPC resources in a multisite database replication configuration. The task is to set up a replicated YottaDB/GT.M database configuration on two servers at two different locations.

Create two databases - America and Brazil - on two different servers ( Server_A and Server_B) and deploy them in a multisite database replication configuration so that America is the primary site and Brazil is the secondary site. Ensure that no YottaDB/GT.M processes exist on either server.

In Server_A and in the directory holding database files for America give the following commands (note that because the default journal pool size is 64MB, a value of 1048576 bytes - YottaDB/GT.M's minimum size of 1MB for this exercise):

.. parsed-literal::
   $ export gtm_repl_instance=multisite.repl 
   $ mupip replicate -instance_create -name=America 
   $ mupip set -replication=on -region "*" 
   $ mupip replicate -source -start -buf=1048576 -secondary=Server_B:1234 -log=A2B.log -instsecondary=Brazil

Now execute the following command:

.. parsed-literal::
   $ gtm_dist/ftok mumps.dat multisite.repl

This command produces the "public" (system generated) IPC Keys (essentially hash values) for mumps.dat and its replication instance multisite.repl. It produces a sample output like the following:

.. parsed-literal::
   mumps.dat :: 721434869 [ 0x2b0038f5 ] 
   multisite.repl :: 721434871 [ 0x2b0038f7 ]

The keys starting with 0x2b (Hexadecimal form) are the keys for the semaphores used by replication instance America with the high order hexadecimal 0x2b replaced by 0x2c for the replication instance file (YottaDB/GT.M's standard prefix for semaphores for journal pools is 0x2c and that for database files is0x2b). You can observe this with the ipcs command:

.. parsed-literal::
   ------ Semaphore Arrays --------
   key  semid owner perms nsems
   0xd74e4524 196608 welsley 660 1
   0x2c0038f7 983041 welsley 777 3
   0x00000000 1015810 welsley 777 5
   0x2b0038f5 1048579 welsley 777 3
   0x00000000 1081348 welsley 777 3

.. note::
   You can expect files in separate file systems to share the same public ftok. This is a normal behavior for large systems with multiple installations and does not affect YottaDB/GT.M operations in any way. This is because YottaDB/GT.M does not assume the semaphore has a one-to-one relationship with the resource and startup/shutdown operations are relatively rare, so the interference among resources have a minimal or no impact. However, the private semaphore (with the 0 key) is unique for a database and is used while a process is actively using the resource.

Execute the following command and note down the shared memory id and private semaphore id on instance America.

.. parsed-literal::
   $ mupip ftok mumps.dat

This command identifies the "private" (YottaDB/GT.M generated) semaphores that a process uses for all "normal" access. The sample output of this command looks like the following:

.. parsed-literal::
   File  ::   Semaphore Id   ::   Shared Memory Id  :: FileId
   ---------------------------------------------------------------------------------------------------------------
   mumps.dat ::  1081348 [0x00108004] :: 2490370 [0x00260002] :: 0xf53803000000000000fe000000000000ffffffd2 

Now, execute the following command and note down the shared memory and private semaphore id for journal pool.

.. parsed-literal::
   $ mupip ftok -jnl multisite.repl

The sample output of this command looks like the following:

.. parsed-literal::
   File   :: Semaphore Id     ::   Shared Memory Id  :: FileId
   ---------------------------------------------------------------------------------------------------------------
   multisite.repl :: 1015810 [0x000f8002]  ::  2457601 [0x00258001] :: 0xf73803000000000000fe000000000000ffffffd2

Note that the Semaphore id 1015810 and Shared Memory ID 2457601 are in the sample output of the ipcs -a command below.

Now execute the command ipcs -a to view the current IPC resources. This command produces an output like the following:

.. parsed-literal::
   ------ Shared Memory Segments --------
   key  shmid owner perms bytes nattch status
   0x00000000 0  root  777 122880 1
   0x00000000 2457601 welsley 777 1048576 1
   0x00000000 2490370 welsley 777 2633728 1
   0x00000000 2523139 welsley 600 393216 2  dest
   0x00000000 2555908 welsley 600 393216 2  dest
   0x00000000 1048583 welsley 600 393216 2  dest
   0x00000000 1081352 welsley 600 393216 2  dest
   0x00000000 1114121 welsley 666 376320 2
   0xd74e4524 1146890 welsley 660 64528 0
   0x00000000 1933323 welsley 666 62500 2
   0x00000000 1966092 welsley 666 1960000 2
   ------ Semaphore Arrays --------
   key  semid owner perms nsems
   0xd74e4524 196608 welsley 660 1
   0x2c0038f7 983041 welsley 777 3
   0x00000000 1015810 welsley 777 5
   0x2b0038f5 1048579 welsley 777 3
   0x00000000 1081348 welsley 777 3
    
   ------ Message Queues --------
   key  msqid owner perms used-bytes messages

Using the following formula, where n is the number of regions, to calculate YottaDB/GT.M's IPC resources in a multisite replication configuration:

.. parsed-literal::
   IPCs = (n regions * (1 shm/region + 1 ftok sem/region + 1 private sem/region)) 
   + 1 sem/journal-pool + 1 sem/receiver-pool 

In this case, America has one region and no receiver-pool so:

.. parsed-literal::
   1 region * 3 IPCs/region + 1 IPC/journal-pool = 4 IPCs

Therefore, assuming that instance America has 1 region, the total IPC utilized by YottaDB/GT.M is: 4 [1 * 3 + 1 +0]. Note that there is no receiver pool for instance America.

.. note::
   For  MUPIP RECOVER operations the total number of IPC resources are 3n (As there is no Journal Pool or Receiver Pool) where  n is the number of regions.

Now connect to Server_B and give the following commands in the directory holding database files for Brazil:

.. parsed-literal::
   $ export gtm_repl_instance=multisite1.repl 
   $ mupip replicate -instance_create -name=Brazil $ mupip rundown -region "*"
   $ mupip set -journal="enable,before,on" -replication=on -region "*"
   $ mupip replicate -source -start -passive -buf=1048576 -log=B2dummy.log -inst=dummy 
   $ mupip replicate -receive -start -listenport=1234 -buf=1048576 -log=BFrA.log

Now execute the command:

.. parsed-literal::
   $gtm_dist/ftok mumps.dat multisite1.repl

This command produces the "public" (system generated) IPC Key of mumps.dat and its replication instance multisite1.repl. It produces a sample output like the following:

.. parsed-literal::
     mumps.dat :: 722134735 [ 0x2b0ae6cf ]
     multisite1.repl :: 722134737 [ 0x2b0ae6d1 ]

Note that keys starting with 0x2b in the output of the ipcs -a command are the public IPC keys for the semaphores of the database file on replication instance Brazil.

Then, execute the following command and note down the shared memory id and private semaphore id on instance Brazil.

.. parsed-literal::
   $ mupip ftok mumps.dat

This command identifies the "private" (YottaDB/GT.M generated) semaphores that a process uses for all "normal" access. The sample output of this command looks like the following:

.. parsed-literal::
   File :: Semaphore Id  :: Shared Memory Id :: FileId
   --------------------------------------------------------------------------------------------------------------
   mumps.dat :: 327683 [0x00050003] :: 11665410 [0x00b20002]:: 0xcfe63400000000000a0000000000000000000000

Now, execute the following command and note down the shared memory and private semaphore id for journal pool.

.. parsed-literal::
   $ mupip ftok -jnl multisite1.repl

The sample output of this command looks like the following:

.. parsed-literal::
   File  :: Semaphore Id  :: Shared Memory Id :: FileId
   ---------------------------------------------------------------------------------------------------------------
   multisite1.repl :: 262145 [0x00040001] :: 11632641[0x00b18001]:: 0xd1e63400000000000a0000000000000000000


Note that the Semaphore id 262145 and Shared Memory ID 11632641 are in the sample output of the ipcs -a command below.

Now, execute the command ipcs -a to view the IPC resources of Brazil.

This command produces a sample output like the following:

.. parsed-literal::
   ------ Shared Memory Segments --------
   key  shmid owner perms bytes nattch status
   0x00000000 11632641 gtmuser 777 1048576 3
   0x00000000 11665410 gtmuser 777 2629632 2
   0x00000000 11698179 gtmuser 777 1048576 2
   ------ Semaphore Arrays --------
   key  semid owner perms nsems
   0x2c0ae6d1 229376 gtmuser 777 3
   0x00000000 262145 gtmuser 777 5
   0x2b0ae6cf 294914 gtmuser 777 3
   0x00000000 327683 gtmuser 777 3
   0x00000000 360452 gtmuser 777 5
   ------ Message Queues --------
   key  msqid owner perms used-bytes messages 

Brazil has 1 region and its receiver server is listening to America, therefore as per the formula for calculating YottaDB/GT.M IPC resources, the total IPCs utilized by YottaDB/GT.M is: 5 [1 * 3 + 1 + 1].

---------------
gtmsecshr
---------------

The YottaDB/GT.M installation script installs gtmsecshr as owned by root and with the setuid bit on. gtmsecshr is a helper program that enables YottaDB/GT.M to manage interprocess communication and clean up interprocess resources. It resides in the $gtm_dist/gtmsecshrdir subdirectory which is readable and executable only by root. gtmsecshr is guarded by a wrapper program. The wrapper program protects gtmsecshr in the following ways:

* It restricts access to gtmsecshr in such a way that processes that do not operate as root cannot access it except though the mechanism used by the wrapper.
* Environment variables are user-controlled input to gtmsecshr and setting them inappropriately can affect system operation and cause security vulnerabilities. While gtmsecshr itself guards against this, the wrapper program provides double protection by clearing the environment of all variables except gtm_dist, gtmdbglvl, gtm_log, and gtm_tmp and truncating those when they exceed the maximum allowed length for the platform.
* gtmsecshr logs its messages in the system log. These messages can be identified with the GTMSECSHR facility name as part of the message. YottaDB/GT.M processes communicate with gtmsecshr through socket files in a directory specified by the environment variable gtm_tmp.

gtmsecshr automatically shuts down after 60 minutes of inactivity. Normally, there is no need to shut it down, even when a system is making the transition between a secondary and a primary. The only occasions when gtmsecshr must be explicitly shut down are when a YottaDB/GT.M version is being removed - either a directory containing the YottaDB/GT.M version the running gtmsecshr process belongs to is being deleted, or when a new YottaDB/GT.M version is being installed in the same directory as an existing one.

.. note::
   YottaDB/FIS strongly recommends against installing a new YottaDB/GT.M version on top of an existing YottaDB/GT.M version.

To terminate a gtmsecshr process, use a KILL-15 after shutting down all YottaDB/GT.M processes and running down all database regions in use by YottaDB/GT.M in that directory.

.. note::
   YottaDB/FIS strongly recommends that all YottaDB/GT.M processes that use a given version of YottaDB/GT.M use the same settings for the gtm_log and gtm_tmp environment variables. gtmsecshr inherits these values from the YottaDB/GT.M process that starts it. Not having common values for gtm_tmp and gtm_log for all processes that use a given version of YottaDB/GT.M can have an adverse impact on performance.

If there are multiple YottaDB/GT.M versions active on a system, YottaDB/FIS recommends different values of gtm_tmp and gtm_log be used for each version. This makes system administration easier.

.. note::
   A given database file can only be open by processes of a single version of YottaDB/GT.M at any given time. Contemporary releases of YottaDB/GT.M protect against concurrent access to YottaDB/GT.M files by processes executing different versions of YottaDB/GT.M. Since historical versions of YottaDB/GT.M did not protect against this condition, YottaDB/FIS recommends procedural safeguards against inadvertent concurrent access by processes of multiple versions on systems on which old versions of YottaDB/GT.M are installed and active, since such concurrent usage can cause structural damage to the database.


