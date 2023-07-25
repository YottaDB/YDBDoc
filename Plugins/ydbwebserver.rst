.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.     #
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

==================
YottaDB Web Server
==================

.. contents::
   :depth: 3

--------
Overview
--------

+++++++++++++
Prerequisites
+++++++++++++

* Knowledge of the `HyperText Transfer Protocol <http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol>`_, especially `request methods <http://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods>`_.
* Passing familiarity with the `M programming language <https://en.wikipedia.org/wiki/MUMPS>`_.

++++++++++++
Installation
++++++++++++

To install the YottaDB Web Server, first download the `YDB-Web-Server repository <https://gitlab.com/YottaDB/Util/YDB-Web-Server>`_ by cloning it with ``git`` or downloading and extracting the `zip file <https://gitlab.com/YottaDB/Util/YDB-Web-Server/-/archive/master/YDB-Web-Server-master.zip>`_.

Then, create a build directory in the root directory of the repository:

.. code-block:: bash

    mkdir build
    cd build

Next, run cmake to generate the Makefiles

.. code-block:: bash

    cmake ..

Finally, install the plugin:

.. code-block:: bash

    [sudo] make install

To uninstall, run the following from the same directory:

.. code-block:: bash

    [sudo] xargs rm < install_manifest.txt

+++++++++++++
Initial setup
+++++++++++++

After installation, set ``ydb_routines`` to contain ``$ydb_dist/plugin/o/_ydbmwebserver.so``, e.g.:

.. code-block:: bash

    export ydb_routines="$ydb_dist/plugin/o/_ydbmwebserver.so $ydb_routines"

Then, you can start the server by running:

.. code-block:: bash

    $ydb_dist/yottadb -run start^%ydbwebreq

You can then stop the server with ``CTRL-C``. Alternatively, you can run the following from another terminal:

.. code-block:: bash

    $ydb_dist/yottadb -run stop^%ydbwebreq

You can then verify the operation of the web server by navigating to `http://localhost:9080/api/ping <http://localhost:9080/api/ping>`_, assuming you started the server without the ``--port`` option. Otherwise, use the specified port instead of ``9080``. For example:

.. code-block:: bash

        $ curl http://localhost:9080/api/ping
    {"self":"94479","server":"94418"}

The returned JSON object contains the child process PID (``"self"``) and the main server process PID (``"server"``).

++++++++++++++++++++++++++++++++++++
Overview of YDB Web Server operation
++++++++++++++++++++++++++++++++++++

The YDB Web Server does two things:

#. Serve pages on the file system from the directory specified by the ``--directory`` start-up parameter, or the current directory by default.
#. Serve web services defined in the ``_ydbweburl.m`` M routine.

The server uses the classic forking model, where the main server process simply forks child processes whenever it receives a connection. Then, the child processes handle all communication with the client(s).

The server listens for a maximum of 5 concurrent connections. There can be as many forked child processes as the operating system allows.

The expected use case for the YDB Web Server is to provide web services from YottaDB and, optionally, to serve web pages used by these web services.

See the `YDB Web Server README.md <https://gitlab.com/YottaDB/Util/YDB-Web-Server/-/blob/master/README.md?ref_type=heads>`_ for how to start and stop the web server, as well as a list of options.

+++++++++++++++++++
Development process
+++++++++++++++++++

These are the steps that you need to more or less take to develop code that
uses the YDB Web Server:

#. Install `YDB-Web-Server <https://gitlab.com/YottaDB/Util/YDB-Web-Server>`_.
#. Source ``$ydb_dist/ydb_env_set`` to set the requisite YottaDB environment variables, including ``$ydb_routines``. You may also set ``$ydb_routines`` manually to include the web server library, which may be in either:

    * M mode: ``$ydb_dist/plugin/o/_ydbmwebserver.so``, or
    * UTF-8 mode: ``$ydb_dist/plugin/o/utf8/_ydbmwebserver.so``
      j
#. Set ``$ydb_routines`` to include a routine directory for storing the routines written in the next two steps. ``$ydb_dist/ydb_env_set`` sets up a ``$ydb_dir/r`` directory by default.
#. Start the Web Server by running the M command: ``do start^%ydbwebreq``
#. Add a URL mapping to your copy of ``_ydbweburl.m`` to map a URL to a ``tag^routine``.
#. Write code in ``tag^routine``.
#. Test the code by using ``curl``.
#. Rewrite and test the code until it works as desired. You do not need to restart the web server when you save a new copy of the routine. After 1 second, the old child process that used the old code dies, so you will get the new copy.

To deploy code to production, you need to do the following steps:

#. Install `YDB-Web-Server <https://gitlab.com/YottaDB/Util/YDB-Web-Server>`_ using the standard install process.
#. Install ``_ydbweburl.m`` and the routine that you wrote above by placing it in the desired directory and including this file or directory in ``$ydb_routines``.
#. Set ``$ydb_routines`` to contain both ``_ydbmwebserver.so`` library files as well as your application code.
#. Start the Web Server. A permanently running server process can be configured using Systemd, if desired.

+++++++++++++++++++++++++++++++++
Serving pages from the filesystem
+++++++++++++++++++++++++++++++++

YDB Web Server serves pages similarly to ``python3 -m http.server``. By default, it serves from the current directory. However, if you start the server with a
``--directory /x/y/z`` argument, the pages will be served from directory ``/x/y/z``.

If you supply the ``--gzip`` command line flag, HTTP responses will be compressed using gzip. If you use ``gzip``, note that it uses ``/dev/shm`` for temporary files. If space is limited, e.g. in Docker images, you may face problems with gzipping, since ``--gzip`` causes extra processing to decrease the size of HTTP responses.

All file system calls send back an `ETag <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag>`_. This way resources will be fetched only once.

+++++++++++++++++++++++
Permanent API endpoints
+++++++++++++++++++++++

The following API endpoints are always accessible via the YottaDB Web Server, regardless of the application:

+ ``/api/ping``: Returns a JSON object containing the worker process PID ("self") and the server process PID ("server").
+ ``/api/version``: Returns a JSON object containing the YottaDB Web Server version number.
+ ``/api/auth-mode``: Returns a JSON object containing a Boolean value of ``true`` if the server was started with a user configuration via the ``--auth-stdin`` or ``--auth-file`` options.
+ ``/api/login``: See the :ref:`authorization` section below.
+ ``/api/logout``: See the :ref:`authorization` section below.

For example:

.. code-block:: bash

    curl http://localhost:9081/api/ping  # Sample output: `{"self":"19341","server":"19338"}`
    curl http://localhost:9081/api/version  # Sample output: `{"version":"4.2.0"}`
    curl http://localhost:9081/api/auth-mode  # Sample output: `{"auth":false}`

++++++++++++++++++++
Serving Web Services
++++++++++++++++++++

The most common use case for the YDB Web Server is to provide web services.

Web services are specified in the ``_ydbweburl.m`` file. You can test the web server using this sample copy of `_ydbweburl.m <https://gitlab.com/YottaDB/Util/YDB-Web-Server/-/blob/master/src/_ydbweburl.m?ref_type=heads>`_. However, this file is not packaged in the default installation, which assumes the user will provide a custom copy of the file with a custom URL mapping.

Here's an excerpt from the sample file:

.. code-block::

    URLMAP ;
     ;;GET /test/xml xml^%ydbwebapi
     ;;POST test/post posttest^%ydbwebapi
     ;;GET test/json getjson^%ydbwebapi
     ;;zzzzz
     ;

For example, an HTTP GET of ``/test/xml`` will execute the code in `xml^%ydbwebapi <https://gitlab.com/YottaDB/Util/YDB-Web-Server/-/blob/master/src/_ydbwebapi.m?ref_type=heads>`_.

.. code-block::

    xml ; GET /test/xml XML sample
     set httprsp("mime")="text/xml"
     set httprsp(1)="<?xml version=""1.0"" encoding=""UTF-8""?>"
     set httprsp(2)="<note>"
     set httprsp(3)="<to>Tovaniannnn</to>"
     set httprsp(4)="<from>Jani</from>"
     set httprsp(5)="<heading>Reminders</heading>"
     set httprsp(6)="<body>Don't forget me this weekend!</body>"
     set httprsp(7)="</note>"
     quit

How to create such code will be explained in the following sections.

~~~~~~~~
HTTP GET
~~~~~~~~

Let's examine how the server figures out which routine to invoke in those simple examples using HTTP GET, starting with the aforementioned entry:

.. code-block::

    ;;GET /test/xml xml^%ydbwebapi

Assuming that your server is listening at ``http://localhost:9080``, navigate your browser to `http://localhost:9080/test/xml <http://localhost:9080/test/xml>`_.

The server will first check the HTTP request type, e.g. ``GET``. Then, it will try to match the path, e.g. ``[/]test/xml``, and derive the routine name, e.g. ``xml``.

In this case, it will run the routine ``xml^%ydbwebapi``. Instructions on how to write such a routine will be provided later.

Now, consider this more complex example:

.. code-block::

    ;;GET test/r/{routine} r^%ydbwebapi

In this case, the server will accept ``GET`` HTTP requests in the variable format ``test/r/routine-name``. In the M code, ``HTTPARGS("routine")`` (or lowercase ``httpargs("routine")``) will contain the value of ``{routine}``. For example, if you call this with ``curl localhost:9080/test/r/XUP``, ``HTTPARGS("routine")`` will be ``XUP``.

Here is yet another, slightly different example:

.. code-block::

 ;;GET test/r/{routine?.1"%25".32AN} r^%ydbwebapi

This matches routine names as long as they fit the pattern of `0-1 %` and `0-32` characters. If a routine doesn't match this pattern, then a 404 error is returned. For example, entering an invalid routine name of ``1AAAA`` will cause a 404 error. ``HTTPARGS``/``httpargs`` will contain the value of ``{routine}`` as before.

With any URL, you can pass HTTP Query Parameters. For example, you can ask for ``curl localhost:9080/test/XUS?format=color``. In this case, ``HTTPARGS`` will contain two values: ``HTTPARGS("routine")="XUS"`` and ``HTTPARGS("format")="color"``.

~~~~~~~~~~~~
POST and PUT
~~~~~~~~~~~~

HTTP verbs ``POST`` and ``PUT`` are used to amend or add data. If you follow a rigid RESTful model, ``POST`` is used to amend data or add data when the location of the additional data is not known. In contrast, ``PUT`` which is used to add or overwrite data when the location of the data is known. Thus a ``POST`` can be used to add data to a database when record numbers are not required, while a ``PUT`` can be used to overwrite the data mapped to a given record number.

Despite the distinction between them, YDB Web Server handles both ``POST`` and ``PUT`` the same way. It's up to the developer adhere to aforementioned the semantics, if desired. For example:

.. code-block::

 ;;POST test/post posttest^%ydbwebapi

Calling ``test/post`` with data will invoke ``posttest^%ydbwebapi``. ``posttest^%ydbwebapi`` does a little processing then returns the data location in an HTTP `Location` header before returning the same data in the body.

~~~~~~~~~~~~~~~~~~~~~
HEAD, DELETE, OPTIONS
~~~~~~~~~~~~~~~~~~~~~

* HTTP ``HEAD`` is internally handled as a ``GET``, except we don't send out the data.
* HTTP ``DELETE`` is supported with the same semantics as a ``PUT``.
* HTTP ``OPTIONS`` is not supported as a verb in ``_ydbweburl.m``, but it's handled internally in the web server to allow for `CORS <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS>`_ access.

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Configuring the YottaDB Web Server environment via HTTP headers
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Some aspects of the YottaDB Web Server environment can be configured using HTTP headers. Specifically:

#. The global directory can be set using the ``X-YDB-Global-Directory`` header.
#. The current working directory can be set using the ``X-YDB-Working-Directory`` header.
#. Various environment variables can be set using the ``X-YDB-Env-Vars`` header.

See the test routine ``_ydbwebtest.m`` in the server source code for examples how to use this advanced functionality.

--------------------------------------------------
Creating and handling a new web server entry point
--------------------------------------------------

For all the following examples, we will use a very simple web service that just multiplies two numbers. The service will handle URLs like the following:

.. code-block::

    /multiply/5/8

In this case, 5 is the multiplier and 8 is multiplicand. The server will multiply these numbers then return the result: 40, in this case.

You will need to add the following to the ``_ydbweburl.m`` file:

.. code-block::

    URLMAP ;
        ;;GET /multiply/{multiplier}/{multiplicand} m^mul

``_ydbweburl.m`` must contain a ``URLMAP`` label, which contains a list of URL entry points defined as M comments. Each entry point must begin with two semicolons ``;;`` followed immediately by the HTTP request type in all capital letters. There should be *no* spaces between the initial two semicolons and the request type.

After the request type comes the URL entry point definition, e.g. ``/multiply/{multiplier}/{multiplicand}``.

After the entry point URL comes the M routine or label that should be called when a request is sent to the entry point URL, e.g. ``m^mul``.

Note that the spelling of the parameter names, e.g. ``multiplier`` and ``multiplicand``,  must match the subscripts referenced by ``httpargs`` in the M routine targeted by the URL, e.g. ``m^mul``.

A sample ``_ydbweburl.m`` file can be found in the ``src`` directory of the ``YDB-Web-Server`` repository.

+++++++++++++++++++++++++++
Creating a handling routine
+++++++++++++++++++++++++++

Here's the multiplier routine, ``mul.m``:

.. code-block::

    mul ; Web Server Math Routine;2014-11-28  5:58 PM
        ;
    m ; multiplication
        ; `httprsp` is where you return the result
        ; `httprsp("mime")` is where you specify the MIME type for the client
        ; If you don't specify a MIME, `application/json; charset=utf-8` is returned.
        ;
        ; Get our arguments
        new m1 set m1=$get(httpargs("multiplier"))
        new m2 set m2=$get(httpargs("multiplicand"))
        ;
        ; If for some reason our arguments are empty, don't go any further
        if (m1="")!(m2="")  do setError^%ydbwebutils(400,"Input parameters are not correct") QUIT
        ;
        set httprsp=m1*m2
        ;
        set httprsp("mime")="text/plain; charset=utf-8"
        ;
        quit

Test this routine by first running it from the command line:

.. code-block::

        YDB>set httpargs("multiplier")=5,httpargs("multiplicand")=40

        YDB>kill httprsp

        YDB>do m^mul

        YDB>zwrite httprsp
        httprsp=200
        httprsp("mime")="text/plain; charset=utf-8"

++++++++++++++++++++++++++++++++++++
Running the code from the Web Server
++++++++++++++++++++++++++++++++++++

To test the multiplier web service, use ``curl`` to submit a request to the multiplier URI:

.. code-block:: bash

        $ curl http://localhost:9081/multiply/5/8
        40

++++++++++++++++++++++++++++++++++++++++
A closer look at routine input variables
++++++++++++++++++++++++++++++++++++++++

The multiply routine is fairly straightforward. However, ``httpargs`` and ``httprsp`` can be explained in greater detail.

~~~~~~~~~~~~~
``httpargs``
~~~~~~~~~~~~~

First, let's examine the ``httpargs`` variable. Recall the URL format for the multiplier routine:

.. code-block::

        multiply/{multiplier}/{multiplicand}

This URL can be accessed by passing literal values for ``multiplier`` and ``multiplicand``, e.g.:

.. code-block::

        multiply/5/8

In this case, the ``httpargs`` variable will contain the following nodes:

.. code-block::

        httpargs("multiplier")=5
        httpargs("multiplicand")=8

You can also pass additional URL query parameters - for example, numeric base - like this:

.. code-block::

        multiply/5/8?base=2

The ``httpargs`` will now have these nodes:

.. code-block::

        httpargs("multiplier")=5
        httpargs("multiplicand")=8
        httpargs("base")=2

Here's a modified routine to handle the base parameter in addition to ``multiplier`` and ``multiplicand``:

.. code-block::

    mul ; Web Server Math Routine;2014-11-28  6:31 PM
        ;
    m   ; multiplication
        ; `httprsp` is where you return the result
        ; `httprsp("mime")` is where you specify the MIME type for the client
        ; If you don't specify a MIME, `application/json; charset=utf-8` is returned.
        ;
        ; Get our arguments
        new m1 set m1=$get(httpargs("multiplier"))
        new m2 set m2=$get(httpargs("multiplicand"))
        new base set base=$get(httpargs("base"))
        ;
        ; If for some reason our httpargs are empty, don't go any further
        if (m1="")!(m2="")  do setError^%ydbwebutils(400,"Input parameters are not correct") QUIT
        ;
        set httprsp=m1*m2
        ;
        if +base set httprsp=$$BASE(httprsp,10,base) ; convert to the requested base
        ;
        set httprsp("mime")="text/plain; charset=utf-8"
        ;
        quit
        ;
    BASE(%X1,%X2,%X3) ;Convert %X1 from %X2 base to %X3 base
        I (%X2<2)!(%X2>16)!(%X3<2)!(%X3>16) Q -1
        Q $$CNV($$DEC(%X1,%X2),%X3)
    DEC(N,B) ;Cnv N from B to 10
        Q:B=10 N N I,Y S Y=0
        F I=1:1:$L(N) S Y=Y*B+($F("0123456789ABCDEF",$E(N,I))-2)
        Q Y
    CNV(N,B) ;Cnv N from 10 to B
        Q:B=10 N N I,Y S Y=""
        F I=1:1 S Y=$E("0123456789ABCDEF",N#B+1)_Y,N=N\B Q:N<1
        Q Y

Test this with ``curl``:

.. code-block::

        $ curl http://localhost:9081/multiply/5/8?base=2
        101000

Expressed in base 10:

.. code-block::

        2**(6-1) + 2**(4-1) = 40.

~~~~~~~~~~~
``httprsp``
~~~~~~~~~~~

The ``httprsp`` argument is simpler to use than ``httpargs``, since it returns a scalar value. For example:

.. code-block::

        set httprsp=5
        set httprsp("mime")="text/plain; charset=utf-8"

It is also possible to use ``httprsp`` to return an array. The simplest way to return an array is to subscript the result with 1,2,3, etc. For example:

.. code-block::

        set httprsp(1)="Mary has"
        set httprsp(2)="a little"
        set httprsp(3)="lamb"
        set httprsp("mime")="text/plain; charset=utf-8"

To transfer a large amount of data, a YottaDB global variable may be preferable, e.g.:

.. code-block::

        set httprsp=$name(^temp($job))
        set @httprsp@(1)="Mary has"
        set @httprsp@(2)="a little"
        set @httprsp@(3)="lamb"
        ; Set more array entries...
        set @httprsp@("mime")="text/plain; charset=utf-8"

For example, consider the ``xml`` label in the ``%ydbwebapi`` routine, which uses a global variable:

.. code-block::

    xml ; GET /test/xml XML sample
        set httprsp("mime")="text/xml"
        set httprsp(1)="<?xml version=""1.0"" encoding=""UTF-8""?>"
        set httprsp(2)="<note>"
        set httprsp(3)="<to>Tovaniannnn</to>"
        set httprsp(4)="<from>Jani</from>"
        set httprsp(5)="<heading>Reminders</heading>"
        set httprsp(6)="<body>Don't forget me this weekend!</body>"
        set httprsp(7)="</note>"
        QUIT

++++++++++++
POST and PUT
++++++++++++

Above we demonstrated how to access the web server using HTTP ``GET`` requests. Now, we'll take a look at ``POST`` and a ``PUT`` requests.

Assume we'd like to store text data in a YottaDB global variable named ``^text``. Each data entry can be subscripted by an entry number, with the matching text stored as the node value under that subscript. For example:

.. code-block::

   ^text(3,1)="It was the best of times"
   ^text(3,2)="and"
   ^text(3,3)="It was the worst of times."

To access this data, let's create two methods on the server, one for ``POST`` and one for ``PUT`` requests.

The ``POST`` method will add text to the next available entry, while the ``PUT`` method will add or replace text for a specific entry. For completeness, a ``GET`` handler is also included in ``_ydbweburl.m``:

.. code-block::

    ;;POST /text post^text
    ;;PUT /text/{ien} put^text
    ;;GET /text/{ien} get^text

Next, let's write a routine for these methods:

.. code-block::

    text ; ven/smh - post and put data into global ^text;2014-11-28  7:37 PM
        ;
    post ; handles POST /text/
        new ien
        set ien=$o(^text(""),-1)+1  ; last sub + 1
        new i for i=0:0 set i=$order(httpreq("body",i)) quit:'i  set ^text(ien,i)=httpreq("body",i) ; put data into text
        set ^text(ien)=$o(^text(ien," "),-1) ; make header node the last sub number in the text
        set httprsp("mime")="text/html; charset=utf-8"
        set httprsp="/text/"_ien
        quit
        ;
    put ; handles PUT /text/{ien}
        new ien set ien=$g(args("ien"))
        if ien<1 do setError^%ydbwebutils(400,"invalid ien") quit ""
        kill ^text(ien) ; bye bye. We are replacing you.
        new i for i=0:0 set i=$order(httpreq("body",i)) quit:'i  set ^text(ien,i)=httpreq("body",i) ; put data into text
        set ^text(ien)=$o(^text(ien," "),-1) ; make header node the last sub number in the text
        set httprsp("mime")="text/html; charset=utf-8"
        set httprsp="/text/"_ien
        quit
        ;
    get ; handles GET /text/{ien}
        new ien set ien=$g(args("ien"))
        if ien<1 do setError^%ydbwebutils(400,"invalid ien") quit
        if '$data(^text(ien)) do setError^%ydbwebutils("404","No such entry exists") quit
        new i for i=1:1:^text(ien) set httprsp(i)=^text(ien,i)
        set httprsp("mime")="text/html"
        quit
        ;

By default, the web server will read 4000 characters per node. For simplicity's sake, we will not parse them according to newline characters.

To test the routine, put some text files into a temporary directory so that they can later be accessed using requests sent via ``curl``. For example:

.. code-block::

        $ ls /tmp/*.txt
        /tmp/gettysburg_address.txt  /tmp/oratio_in_l_catilinam_para.txt  /tmp/varsari_da_vinci.txt

First, try a ``POST`` request, e.g.:

.. code-block::

        $ curl -X POST --data-binary @gettysburg_address.txt http://localhost:9081/text

        HTTP/1.1 201 Created
        Date: Sat, 29 Nov 2014 00:50:29 GMT
        Location: https://localhost:9081/text/1
        Content-Type: application/json; charset=utf-8
        Content-Length: 7

        /text/1

This will result in the following additions to the YottaDB database:

.. code-block::

        ^text(1)=1
        ^text(1,1)="Four score and seven years ago our fathers brought forth on this con
                          tinent a new nation, conceived in liberty, and dedicated to the propos
                          ition that all men are created equal."_$C(10,10)_"Now we are engaged i
                          n a great civil war, testing whether that nation, or any nation so con
                          ceived and so dedicated, can long endure. We are met on a great battle
                          field of that war. We have come to dedicate a portion of that field, a

Next, try  a ``PUT`` request:

.. code-block::

        $ curl -X PUT --data-binary @varsari_da_vinci.txt http://localhost:9081/text/5

        HTTP/1.1 201 Created
        Date: Sat, 29 Nov 2014 00:56:23 GMT
        Location: https://localhost:9081/text/5
        Content-Type: application/json; charset=utf-8
        Content-Length: 7

        /text/5

This will result in the following database additions:

.. code-block::

        ^text(5)=4
        ^text(5,1)=" LIFE OF LEONARDO DA VINCI: Painter and Sculptor of Florence"_$C(10,10)_"The greatest gifts are often seen, in the course of nature, rained by celestial influences on human creatures; and sometimes, in supernatural fashion, beauty, grace, and talent are united beyond measure in.... and I have one, a head drawn with"
        ^text(5,2)=" the style in chiaroscuro, which is divine."_$C(10,10)_"And there was infused in that brain such grace from God, and a power of expression in such sublime accord with the intellect and memory that served it, and he knew so well how to express his conceptions by draughtmanship, that he vanquished with his discourse, and confuted with his reasoning... him, not thinking himself capable of imagining features that should"
        ^text(5,3)=" represent the countenance of him who, after so many benefits received, had a mind so cruel as to resolve to betray his Lord, the Creator of the world. However, he would seek out a model for the latter; but if in the end he could not find a better, he should not want that of th...."

The new URL of the saved data is returned with each ``POST`` or ``PUT`` request. Each new URL can then be used with ``GET``, e.g.:

.. code-block::

        $ curl http://localhost:9081/text/5

        LIFE OF LEONARDO DA VINCI: Painter and Sculptor of Florence

        The greatest gifts are often seen, in the course of nature, rained by celestial influences on human creatures; ...

Continuing with the example, let's try to store Cicero's speech using a ``POST`` request, which we expect will go into slot 6, since the last entry was stored in slot 5 using ``PUT`` request:

.. code-block::

        $ curl -X POST --data-binary @oratio_in_l_catilinam_para.txt http://localhost:9081/text

        HTTP/1.1 201 Created
        Date: Sat, 29 Nov 2014 01:07:23 GMT
        Location: https://localhost:9081/text/6
        Content-Type: application/json; charset=utf-8
        Content-Length: 7

        /text/6

The database will now contain these nodes:

.. code-block::

        ^text(6)=6
        ^text(6,1)=" [1] I. Quo usque tandem abutere, Catilina, patientia nostra?
        quam diu etiam furor iste tuus nos eludet? quem ad finem sese effrenata iactabit
        audacia? Nihilne te nocturnum praesidium Palati, nihil urbis vigiliae, nihil
        timor populi, nihil con cursus bonorum omnium, nihil hic munitissimus habendi
        senatus locus, nihil horum ora voltusque moverunt? Patere tua consilia non
        sentis, constrictam iam horum omnium scientia teneri coniurationem tuam non
        vides? Quid proxima, quid superiore noct...

Now, let's try an error case by attempting to ``GET`` data that doesn't exist:

.. code-block::

        $ curl http://localhost:9081/text/10
        {"apiVersion":1.1,"error":{"code":404,"errors":[{"errname":"Unknown error","message":"150379354,filesys+12^%ydbwebapi,%YDB-E-DEVOPENFAIL, Error opening \/tmp\/text\/10,%SYSTEM-E-ENO2, No such file or directory","reason":500},{"errname":"Not Found","message":"Not Found","reason":404}],"request":"GET \/text\/10 ","toperror":"Not Found"}}

The server in this case returns a 404 error as expected. Now, try to ``PUT`` to an invalid location:

.. code-block::

        $ curl -X PUT --data-binary @varsari_da_vinci.txt http://localhost:9081/text

        HTTP/1.1 404 Not Found
        Date: Sat, 29 Nov 2014 01:15:47 GMT
        Content-Type: application/json; charset=utf-8
        Content-Length: 156

        {"apiVersion":1.1,"error":{"code":404,"errors":[{"errname":"Unknown error","message":"150379354,filesys+12^%ydbwebapi,%YDB-E-DEVOPENFAIL, Error opening \/tmp\/text,%SYSTEM-E-ENO2, No such file or directory","reason":500},{"errname":"Not Found","message":"Not Found","reason":404}],"request":"PUT \/text ","toperror":"Not Found"}}

This also results in a 404 error, as expected.

.. _testing:

-------
Testing
-------

+++++++++++++++++
Automated Testing
+++++++++++++++++

``%ydbwebtest`` is the main testing routine. Note that it requires the `libcurl plugin <https://github.com/shabiel/fis-gtm-plugins/tree/master/libcurl>`_.

The testing system requires some set-up; it's easiest to do it via the Dockerfile like this:

.. code-block:: bash

    $ docker build -t mws .
    $ docker run -v $PWD/src:/mwebserver/r --rm mws tests
     -------------------------------- %ydbwebtest --------------------------------
    tstartagain - Start again on the same port--------------------  [OK]  104.514ms
    tdebug - Debug Entry Point------------------------------------  [OK]  125.257ms
    thome - Test Home Page----------------------------------------  [OK]    7.989ms
    tgetr - Test Get Handler Routine------------------------------  [OK]    5.443ms
    tputr - Put a Routine-----------------------------------------  [OK]    9.604ms
    tgetxml - Test Get Handler XML--------------------------------  [OK]    4.628ms
    tdecodeutf8 - Test Decode UTF-8 URL---------------------------  [OK]    4.474ms
    tencdecutf8 - Encode and Decode UTF-8-------------------------  [OK]    0.198ms
    tencdecx - Encode and Decode an excepted character------------  [OK]    0.046ms
    tpostutf8 - Post UTF8 data, expect parts of url post data back  [OK]    5.640ms
    tgzip - Test gzip encoding------------------------------------  [OK]  130.423ms
    tnogzip - Test the default nogzip-----------------------------  [OK]    8.432ms
    temptynogzip - Empty response with no gzip encoding-----------  [OK]    4.935ms
    temptygzip - Empty response with gzip-------------------------  [OK]    4.851ms
    tping - Ping--------------------------------------------------  [OK]    5.873ms
    terr - generating an error------------------------------------  [OK]    6.098ms
    terr2 - crashing the error trap-------------------------------  [OK]    4.460ms
    tcustomError - Custom Error-----------------------------------  [OK]    5.330ms
    tlong - get a long message------------------------------------  [OK]    5.441ms
    tDC - Test Disconnecting from the Server w/o talking----------  [OK]  100.330ms
    tInt - ZInterrupt---------------------------------------------  [OK]  113.296ms
    tLog1 - Set httplog to 1--------------------------------------  [OK]  221.629ms
    tLog2 - Set httplog to 2--------------------------------------  [OK]  118.528ms
    tLog3 - Set httplog to 3--------------------------------------  [OK]  225.367ms
    tDCLog - Test Log Disconnect----------------------------------  [OK]  212.880ms
    tOptionCombine - Test combining options (#113)----------------  [OK]  123.547ms
    tWebPage - Test Getting a web page----------------------------  [OK]  122.532ms
    tHomePage - Getting index.html page---------------------------  [OK]  120.029ms
    CORS - Make sure CORS headers are returned--------------------  [OK]    8.040ms
    login - Test that logging in/tokens/logging out works---------  [OK]  940.353ms
    tTokenCleanup - Test Token Cleanup with timeout---------------  [OK] 1461.319ms
    tLoginNoTimeout - Test Logins with no Timeouts----------------  [OK]  460.297ms
    tLoginMultipleServers - Test login with multiple servers------  [OK]  682.582ms
    tusersNoFile - Test --auth-file with a file that doesn't exist  [OK]   24.636ms
    tusersInvalidJSON - Test --auth-file with a invalid JSON------  [OK]   25.151ms
    tusersValidJSONInvalidKeys - Test --auth-file with bad keys---  [OK]   15.090ms
    tsodiumerror - Test crashing libsodium runtime----------------  [OK]   10.344ms
    tauthMode - /api/auth-mode------------------------------------  [OK]  241.926ms
    tpost - simple post-------------------------------------------  [OK]    8.399ms
    tgetjson - Get simple JSON (tests auto-encoder)---------------  [OK]    5.573ms
    tpostmalformed - Malformed post-------------------------------  [OK]    5.862ms
    tTLS - Start with TLS and testPort 55730 is currently being used.
    Checking if it is the YDB-Web-Server.
    Using TLS. $DEVICE: 0
    HTTP/1.1 200 OK
    {"self":"395","server":"386"}
    Server running at 386
    Now going to stop it...
    STOP issued to process 386
    Deleting tokens database files (if present)
    --------------------------------------------------------------  [OK]  440.622ms
    tEtag1 - Test caching with Etag-------------------------------  [OK]    9.201ms
    tReadWrite - Test read-write flag-----------------------------  [OK]  125.115ms
    tVersion - version--------------------------------------------  [OK]    8.435ms
    tUppercase - uppercase HTTP variables-------------------------  [OK]    7.357ms
    tGlobalDir - Custom Global Directory using X-YDB-Global-Directory
     -------------------------------------------------------------  [OK]   43.380ms
    tStop - Stop the Server. MUST BE LAST TEST HERE.Port 55728 is currently being used.
    Checking if it is the YDB-Web-Server.
    HTTP/1.1 200 OK
    {"self":"456","server":"20"}
    Server running at 20
    Now going to stop it...
    STOP issued to process 20
    Deleting tokens database files (if present)
    --------------------------------------------------------------  [OK]  109.430ms

     --------------------------- %ydbwebjsonEncodeTest ---------------------------
    numeric - is numeric function---------------------------------  [OK]    0.076ms
    nearzero - encode of numbers near 0---------------------------  [OK]    0.188ms
    jsonesc - create JSON escaped string--------------------------  [OK]    0.134ms
    basic - encode basic object as JSON---------------------------  [OK]    0.909ms
    vals - encode simple values only object as JSON---------------  [OK]    0.289ms
    long - encode object with continuation nodes for value--------  [OK]    0.825ms
    pre - encode object where parts are already JSON encoded------  [OK]    0.190ms
    wp - word processing nodes inside object----------------------  [OK]    0.757ms
    ltzero - leading / trailing zeros get preserved---------------  [OK]    0.128ms
    strings - force encoding as string----------------------------  [OK]    0.099ms
    labels - unusual labels---------------------------------------  [OK]    0.216ms
    example - encode samples that are on JSON.ORG-----------------  [OK]    3.957ms
    keyesc - keys should be escaped-------------------------------  [OK]    0.060ms
    extarray - No top object; first level is an array-------------  [OK]    0.157ms

     --------------------------- %ydbwebjsonDecodeTest ---------------------------
    jsonues - unescape JSON encoded string------------------------  [OK]    0.036ms
    splita - JSON input with escaped characters on single line (uses build)
     -------------------------------------------------------------  [OK]    0.458ms
    splitb - multiple line JSON input with lines split across tokens (uses builda)
     -------------------------------------------------------------  [OK]    0.115ms
    splitc - multiple line JSON input with lines split inside boolean value
     -------------------------------------------------------------  [OK]    0.151ms
    splitd - multiple line JSON input with key split--------------  [OK]    0.056ms
    long - long document that must be saved across extension nodes  [OK] 51044.846ms
    frac - multiple lines with fractional array elements----------  [OK]    0.135ms
    valonly - passing in value only -- not array------------------  [OK]    0.144ms
    numeric - passing in numeric types and strings----------------  [OK]    0.070ms
    nearzero - decoding numbers near 0----------------------------  [OK]    0.072ms
    badquote - poorly formed JSON (missing close quote on LABEL)--  [OK]    0.134ms
    badslash - poorly formed JSON (non-escaped backslash)---------  [OK]    0.130ms
    badbrace - poorly formed JSON (Extra Brace)-------------------  [OK]    0.046ms
    badcomma - poorly formed JSON (Extra Comma)-------------------  [OK]    0.038ms
    psnum - subjects that look like a numbers shouldn't be encoded as numbers
     -------------------------------------------------------------  [OK]    0.137ms
    numlabel - label that begins with numeric---------------------  [OK]    0.242ms
    purenum - label that is purely numeric------------------------  [OK]    0.409ms
    strtypes - strings that may be confused with other types------  [OK]    0.062ms
    estring - a value that looks like an exponents, other numerics  [OK]    0.213ms
    sam1 - decode sample 1 from JSON.ORG--------------------------  [OK]    0.195ms
    sam2 - decode sample 2 from JSON.ORG--------------------------  [OK]    1.747ms
    sam3 - decode sample 3 from JSON.ORG--------------------------  [OK]    1.398ms
    sam4 - decode sample 4 from JSON.ORG--------------------------  [OK]   21.893ms
    sam5 - decode sample 5 from JSON.ORG--------------------------  [OK]    2.636ms
    maxnum - encode large string that looks like number-----------  [OK]    0.420ms
    escq - escaped quote across lines-----------------------------  [OK]    0.125ms
    keyquote - keys with quotes-----------------------------------  [OK]    0.058ms

    Ran 3 Routines, 89 Entry Tags
    Checked 300 tests, with 0 failures and encountered 0 errors.

-------------------------------------------------
Debugging code written for the YottaDB Web Server
-------------------------------------------------

The YottaDB Web Server provides a ``--debug`` option for setting breakpoints to assist in debugging web application code. ``--debug`` sets a breakpoint at the specified label name, such that web server execution will break and present an interactive YottaDB prompt when that label is about to be executed. For example:

.. code-block::

    Window 1$ yottadb -r %ydbwebreq --debug ping^%ydbwebapi
    Starting Server at port 9080 in directory /home/sam/work/gitlab/MWS/ at logging level 0 in debug mode stopping at ping^%ydbwebapi
    Window 2$ curl localhost:9080/api/ping
    Window 1:
    %YDB-I-BREAKZBA, Break instruction encountered during ZBREAK action
                    At M source location ping+1^%ydbwebapi
    %YDB-W-NOTPRINCIO, Output currently directed to device /dev/null

    YDB>u 0

    YDB>zwrite
    %ydbnull="/dev/null"
    %ydbtcp="SCK$9080"
    HTTPHASUSERS=0 ;*
    HTTPREADWRITE=0 ;*
    HTTPREQ("header")="upgrade-insecure-requests"
    HTTPREQ("header","accept")="text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"
    HTTPREQ("header","accept-encoding")="gzip, deflate"
    HTTPREQ("header","accept-language")="en-US,en;q=0.5"
    HTTPREQ("header","connection")="keep-alive"
    HTTPREQ("header","host")="zundert.yottadb.local:9080"
    HTTPREQ("header","upgrade-insecure-requests")=1
    HTTPREQ("header","user-agent")="Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/116.0"
    HTTPREQ("method")="GET"
    HTTPREQ("path")="/api/ping"
    HTTPREQ("query")=""
    d=""
    *httpargs=HTTPARGS
    httperr=0
    *httphasusers=HTTPHASUSERS
    httplog=0
    httpoptions("auth-file")=""
    httpoptions("auth-stdin")=0
    httpoptions("debug")="ping^%ydbwebapi"
    httpoptions("directory")="/home/sam/work/gitlab/MWS/"
    httpoptions("gzip")=0
    httpoptions("log")=0
    httpoptions("port")=9080
    httpoptions("readwrite")=0
    httpoptions("tlsconfig")=""
    httpoptions("token-timeout")=900
    httpparentpid=10156
    *httpreadwrite=HTTPREADWRITE
    httpremoteip="::ffff:10.0.9.3"
    httpreq("header")="upgrade-insecure-requests"
    httpreq("header","accept")="text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"
    httpreq("header","accept-encoding")="gzip, deflate"
    httpreq("header","accept-language")="en-US,en;q=0.5"
    httpreq("header","connection")="keep-alive"
    httpreq("header","host")="zundert.yottadb.local:9080"
    httpreq("header","upgrade-insecure-requests")=1
    httpreq("header","user-agent")="Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/116.0"
    httpreq("method")="GET"
    httpreq("path")="/api/ping"
    httpreq("query")=""
    httpttimeout=900000000
    k="CONNECT|h1692284316000|::ffff:10.0.9.3"
    libsodiumFound=0
    parentStdout="/proc/10156/fd/1"
    parentStdoutAvailable=1
    routine="ping^%ydbwebapi"
    t=0
    tcpx=""

    YDB>u "/dev/null"

    YDB>zc
    Window 1: {"self":"23984","server":"23984"}

++++++++++++++++++++++++++++++
Utility routines for debugging
++++++++++++++++++++++++++++++

The YottaDB Web server also provides two utility routines for help when debugging API code:

.. code-block::

    do stdoutzw^%ydbwebutils("myvariable")  ; ZWRITEs the contents of the given M variable, e.g. `myvariable`.
    ; Prints the given string to stdout. M variables may be output by being passed directly or by concatenating them to the string with the `_` operator.
    do stdout^%ydbwebutils("String to print")

To print various levels of application logging to standard output, you can combine the above with the ``httplog`` variable, which corresponds to ``--log n`` sent via the command line, where ``n`` is 0 to 4.

-------------------------------------
Using the YDB Web Server with Systemd
-------------------------------------

++++++++++++
Introduction
++++++++++++

The YDB web server starts and runs in the foreground until a signal 2 (CTRL-C) or a signal 15, i.e. `MUPIP STOP <https://docs.yottadb.com/AdminOpsGuide/dbmgmt.html#stop>`_ is received. To run it in the background, you can use your shell's job control or `systemd <https://systemd.io/>`_.

++++++++++++++
Systemd Set-up
++++++++++++++

To run the YDB Web Server in the background using Systemd, first create a ``/lib/systemd/system/ydb-web-server.service`` file like this:

.. code-block::

    [Unit]
    Description=YottaDB Web Server
    After=network.target

    [Service]
    Type=exec
    User=xxx
    Environment='ydb_dist=/usr/local/lib/yottadb/r138'
    Environment='ydb_routines=$ydb_dist/plugin/o/_ydbmwebserver.so $ydb_dist/libyottadbutil.so'
    ExecStart=/usr/bin/env "${ydb_dist}/yottadb" -run start^%%ydbwebreq --directory /var/www --port 9080 --log 1
    ExecStop=/usr/bin/env "${ydb_dist}/yottadb" -run stop^%%ydbwebreq --port 9080
    Restart=on-failure
    StandardOutput=tty

Replace the paths with paths appropriate to your system. Note that there are many other ways to do this, e.g. using `EnvironmentFile` for YottaDB environment variables or offloading the entire setup process to a script.

After creating the ``ydb-web-server.service`` file, do the following:

.. code-block::

    systemctl daemon-reload
    systemctl enable ydb-web-server
    systemctl is-enabled ydb-web-server
    systemctl status ydb-web-server
    systemctl start ydb-web-server
    systemctl status ydb-web-server

This will to load the service file, enable the service (i.e. start it on reboot), check whether the service is enabled, start it, then check whether it was started successfully.

You can also try the following as well:

.. code-block::

    systemctl stop ydb-web-server
    systemctl restart ydb-web-server
    journalctl -xeu ydb-web-server

.. _tlssetup:

---------------------
TLS Set-up on YottaDB
---------------------

Setting up TLS can be difficult. The following instructions are provided as a guide, though they are not guaranteed to work in any particular case.

+++++++
Install
+++++++

Follow the instructions for `YDBEncrypt <https://gitlab.com/YottaDB/Util/YDBEncrypt>`_.

++++++++++++++++++
Certificate Set-up
++++++++++++++++++

.. code-block:: bash

    # Go to your database
    cd /data

    # Create your certificate with a key that has a password. I know from previous
    # interaction with the GT.M developers is that they don't allow passwordless keys
    # for business reasons. Here's is how I did it; but you may already have a
    # certificate. I moved all the files into a cert directory after this.
    openssl genrsa -aes128 -passout pass:monkey1234 -out ./mycert.key 2048
    openssl req -new -key ./mycert.key -passin pass:monkey1234 -subj '/C=US/ST=Washington/L=Seattle/CN=www.smh101.com' -out ./mycert.csr
    openssl req -x509 -days 365 -sha256 -in ./mycert.csr -key ./mycert.key -passin pass:monkey1234 -out ./mycert.pem
    mkdir certs
    mv mycert.* certs/

    # Create a file (name doesn't matter) called ydbcrypt_config.libconfig with the
    # following contents. Note the section called server. This can be called anything.
    # It lets you put a pair of cert/key for each environment you need to configure.
    # Note the "client" section. This allows you to use the self-signed certificate
    # by telling YottaDB about it.
    cat ydbcrypt_config.libconfig
    tls: {
      server: {
        format: "PEM";
        cert: "/data/certs/mycert.pem";
        key:  "/data/certs/mycert.key";
      };
      client: {
        CAfile: "/data/certs/mycert.pem";
      };
    }

    # In your file that sets up the YottaDB environment, add set the env variable
    # ydbcrypt_config to be the path to your config file:
    export ydbcrypt_config="/data/ydbcrypt_config.libconfig"

    # Find out the hash of your key password using the maskpass utility
    $ydb_dist/plugin/ydbcrypt/maskpass <<< 'monkey1234' | cut -d ":" -f2 | tr -d ' '

    # In your environment file, ydbtls_passwd_{section name} to be that hash. For me, it's:
    export ydbtls_passwd_server="30A22B54B46618B4361F"

    # Run the server like this, substituting the {section name} appropriately. Here it is server. See how to stop it below (although you can CTRL-C here and stop it).
    $ydb_dist/yottadb -run ^%ydbwebreq --port 9080 --tlsconfig server

    # Test the server like this (cacert to supply curl with the self-signed Certificate)
    curl --cacert /data/certs/mycert.pem https://localhost:9080

Then, from M, you can connect to the server like this, implicitly using the self-signed certificate from via the ``client.CAfile`` in the section above:

.. code-block::

    set port=9080
    open "porttest":(connect="127.0.0.1:"_port_":TCP":delim=$char(13,10):attach="client"):0:"SOCKET"
    write /tls("client",,"client")
    set d=$device ; check d to see if it is positive--in that case, TLS failed.
    write "GET /api/ping HTTP/1.1"_$char(13,10)
    write "Host: localhost:"_options("port")_$char(13,10)
    write "User-Agent: "_$zposition_$char(13,10)
    write "Accept: */*"_$char(13,10)_$char(13,10)
    new httpstatus read httpstatus
    ; etc.

Log output will look something like this:

.. code-block::

    Starting Server at port 9080 using TLS configuration server
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:52 PM] Starting Child at PID 13 from parent 1
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:52 PM] TLS Connection Data:
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:52 PM]             $DEVICE: 1,Connection reset by peer
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:52 PM]                $KEY: ESTABLISHED|h1663247992000|::ffff:172.17.0.1
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:52 PM]               $TEST: 0
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:52 PM] Disconnect/Halt 13
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM] Starting Child at PID 15 from parent 1
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM] TLS Connection Data:
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM]             $DEVICE: 0
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM]                $KEY: ESTABLISHED|h1663247998000|::ffff:172.17.0.1
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM]               $TEST: 1
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM] GET / HTTP/1.1
    ::ffff:172.17.0.1 - - [15/SEP/2022 01:19:58 PM] Disconnect/Halt 15

To stop TLS, use ``--tlsconfig client``, e.g.:

.. code-block::

    $ydb_dist/yottadb -run stop^%ydbwebreq --port 9080 --tlsconfig client

.. _authorization:

--------------------------------
Authentication and Authorization
--------------------------------

Before starting, note that ``libsodium-devel`` must be installed on your server in order to use the features here. If ``libsodium-devel`` wasn't installed it prior to installing the `YottaDB Web Server <https://gitlab.com/YottaDB/Util/YDB-Web-Server>`_, please install it and then reinstall the Web Server.

++++++++++++++
Authentication
++++++++++++++

When you start the server with ``start^%ydbwebreq``, the server will not require any authentication, and all web services will be accessible by anyone with access to your network. To protect your web services, enable authentication.

Note that file system pages are NEVER protected by authentication: only web services defined in your ``_ydbweburl.m`` file are protected. This is the default configuration, since file pages must be served in order to prompt users to log in.

There are currently two ways to add authentication:

#. Using the ``--auth-stdin`` flag.

    * Prompts for a username, password, and role. Multiple users may be entered if desired.
    * Users will be saved into a ``users.json`` file in the current directory, which can then be used with the ``--auth-file </path/to/filename.json>`` flag.
#. Using the ``--auth-file </path/to/filename.json>`` option.

    * Loads the users in ``/path/to/filename.json``. The file has JSON content and the path can be absolute or relative.

        * Passwords in this file cannot start with a ``$``, since that character is used to identify hashed passwords (this restriction does not apply when using the ``--auth-stdin`` flag).
        * Any file at ``/path/to/filename.json`` must be in the following format if created manually:

.. code-block:: json

    [
        {
            "username": "user1",
            "password": "plaintext-password1",
            "authorization": "RW"
        },
        {
            "username": "user2",
            "password": "plaintext-password2",
            "authorization": "RO"
        }
    ]

When the server starts up and reads the newly created JSON file, the passwords will be hashed and the plain-text passwords will no longer exist.

For example:

.. code-block:: bash

    $ydb_dist/yottadb -r %ydbwebreq --auth-stdin

    Please enter usernames, passwords, authorization at the prompts:
    Enter enter without entering a username to quit from the loop.

    Username: sam
    Password: foo
    Authorization: RW

    Username: <enter>
    Saving users to file users.json with passwords hashed
    Starting Server at port 9080 in directory xxx at logging level 0 using authentication

Also:

.. code-block:: bash

    $ydb_dist/yottadb -r %ydbwebreq --auth-file users.json
    Starting Server at port 9080 in directory xxx at logging level 0 using authentication

If you manually create a `users.json` file as shown above, you will get a message about each password getting hashed:

.. code-block:: bash

    $ydb_dist/yottadb -r %ydbwebreq --auth-file myusers.json
    Hashing password for user user1
    Hashing password for user user2
    Starting Server at port 9080 in directory xxx at logging level 0 using authentication

~~~~~~~~~~~~~~~~~~~~~~~~~~~
Login/Token/Logout workflow
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once authentication is enabled, all REST endpoints defined in the ``_ydbweburl.m`` file will be protected. However, these endpoints are always available from the server at these addresses:

* ``/api/ping``
* ``/api/version``
* ``/api/login``
* ``/api/logout``
* ``/api/auth-mode``

Attempts to call any other end point without authentication or with a bad token will result in an error. For example:

.. code-block::

    curl -Ss localhost:9080/test/json | jq
    {
      "apiVersion": 1.1,
      "error": {
        "code": 403,
        "errors": [
          {
            "errname": "Forbidden",
            "message": "Forbidden",
            "reason": 403
          }
        ],
        "request": "GET /test/json ",
        "toperror": "Forbidden"
      }
    }

To login, POST a JSON of the format: ``{ "username": "xxx", "password": "xxx" }`` to ``/api/login``. The server will return a token in the body of the request in the format: ``{ "token": "xxx", "authorization": "RO", timeout:900 }``. It will return ``401 Unauthorized`` if the username and password are not specified correctly.

For example:

.. code-block::

    curl -H 'Content-Type: application/json' -d '{ "username": "sam", "password": "foo" }' localhost:9080/api/login
    {"authorization":"RW","timeout":900,"token":"F3joHQj0kyt1Df8ZglOp40"}

To terminate the session and invalidate the token, log out by sending the token back in the ``Authorization`` request header using a ``GET`` call.

.. code-block::

    $ curl -H 'Authorization: Bearer F3joHQj0kyt1Df8ZglOp40' localhost:9080/api/logout
    {"status":"OK"}

It is possible to log out again and receive an HTTP 200 in response, but the ``status`` will say ``token not found``.

If a token timed out, you will get an HTTP 408 back, with a message of "Token timeout". By default, each token will time out in 15 minutes from its last use.

Tokens are cleaned at 10 times the timeout. In the default case, they will be cleaned in 150 minutes from the last time the token is used. In this case, you will get a 403 with a message of "Forbidden".

The default timeout can be changed by using ``--token-timeout``.  See below for more details.

+++++++++++++
Authorization
+++++++++++++

Currently, nothing is done with the authorization of RO or RW except to populate the `HTTPREADWRITE` variable. It's the responsibility of the end application choose how to handle the value of this variable. If authorizations other than "RW" are used, `HTTPREADWRITE` will remain zero.

++++++++++++++++++++++++++++
Miscellaneous considerations
++++++++++++++++++++++++++++

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using ``--token-timeout {n}``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``--token-timeout {n}`` flag can be used to specify when tokens obtained via a log-in exchange will expire. ``n`` is the number of seconds.

The default token timeout is 15 minutes. Specifying ``--token-timeout 0`` will run the server with no timeouts. This can be useful for machine-to-machine communication where no timeout behavior is desired.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Debugging token issues in development
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The YottaDB Web Server supports a ``--log`` option for logging web server activity during operation. ``--log`` supports five levels of verbosity using the values 0 through 4:

#. ``0``: No output except first line mentioning the server and its start-up settings, including, importantly, the port number.
#. ``1``: Worker process start, TLS connection information, HTTP requests, HTTP Continues, Worker process stop, Use of alternate global directory, working directory, or environment variables, token database location
#. ``2``: Log headers, periodic token clean-up message
#. ``3``: Log all input and output from server, print out authentication tokens
#. ``4``: Single line stepping (experimental)

Each successive verbosity level includes all output from preceding verbosity levels. For example:

Log level 1 show the location of the created database:

.. code-block::

    <PARENT> - - [14/APR/2023 12:08:08 PM] Created database - global directory: /tmp/yottadb/r999_x86_64/ydbgui94468.gld
    <PARENT> - - [14/APR/2023 12:08:08 PM]                  - database file   : /tmp/yottadb/r999_x86_64/ydbgui94468.dat

Log level 2 additionally shows every timeout interval (15 minutes by default) before each request:

.. code-block::

    <PARENT> - - [14/APR/2023 12:19:28 PM] Cleaning Tokens

Log level 3 additionally shows sensitive information on all user hashes and tokens after every 10 seconds of inactivity:

.. code-block::

    <PARENT> - - [14/APR/2023 12:11:08 PM] Users
    ^users("d6AyoeTJ7tSyz21TuGsw0E")="RW"
    <PARENT> - - [14/APR/2023 12:11:08 PM] Tokens
    ^tokens("v6rLcA6VSsd7IHtGWzkD6B")="1681488658746732^RW"
    ^tokensByTime(1681488658746732,"v6rLcA6VSsd7IHtGWzkD6B")=""

Note that passwords and password-hashes are never printed, since they are not actually stored anywhere.

Log level 4 prints the M lines that the server executes:

.. code-block::

     sstep+4^%ydbwebutils: open parentStdout use parentStdout
     sstep+5^%ydbwebutils: write !,"Stepping STARTED",!

~~~~~~~~~~~~~~~~~~~~~~
The `--readwrite` flag
~~~~~~~~~~~~~~~~~~~~~~

If the server is started with users, the ``--readwrite`` flag no longer applies and the ``HTTPREADWRITE`` variable is set *only if* the authorization is "RW".

You are free to have different authorizations which you can obtain at runtime using the API ``$$getAuthorizationFromToken^%ydbwebusers(token)``.

--------------------------------------
Running the YDB Web Server with Docker
--------------------------------------

To build a YDB Web Server Docker image and run tests, run:

.. code-block::

    docker build -t mws .

Various options can be used to change the behavior of this command:

.. code-block::

    # Run Server on port 9080
    docker run -v $PWD/src:/mwebserver/r --rm -it -p 9080:9080 mws server

    # Run Server on port 9080 with level 2 verbosity
    docker run -v $PWD/src:/mwebserver/r --rm -it -p 9080:9080 mws server 2

    # Run Tests
    docker run -v $PWD/src:/mwebserver/r --rm mws tests

    # Run Bash
    docker run -v $PWD/src:/mwebserver/r --rm -it -p 9080:9080 mws bash

    # Run debugger (starts server on 9080, and you need to zstep into)
    docker run --rm -it -p 9080:9080 mws debug

    # Run Server TLS on port 9080:
    docker run -p 9080:9080 -v $PWD/src:/mwebserver/r --rm -it mws server-tls

    # Run Debug TLS on port 9080:
    docker run -p 9080:9080 -v $PWD/src:/mwebserver/r --rm -it mws debug-tls

Since you are passing the ``src`` directory in Docker run as a volume, you can modify the routines on the host and see the changes in the container right away.

---------
Utilities
---------

The primary input and output format for the web services is JSON. In the preceding sections, most examples sent text output to web service calls.

Now, let's demonstrate how to use JSON for web service I/O as well as a few other helpful resources and techniques, including:

* API calls that help verify that all required data is sent
* How to parse text by new lines
* How to send error messages back to the client

++++
JSON
++++

Both JSON encoding and decoding are done automatically by the YottaDB web server if the ``accept-encoding`` request header is "application/json" or the ``content-type`` response reader is "application/json". You can also use the JSON encoding/decoding APIs directly if you wish.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How to use Auto Encoding of JSON
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Encoding is done automatically when sending data from M to the browser. For example, in ``_ydbweburl.m``:

.. code-block::

    ;;GET test/json getjson^myjson

In ``myjson.m``:

.. code-block::

    getjson ; GET /test/json JSON sample
        set httprsp("foo",1)="boo"
        set httprsp("foo",2)="doo"
        set httprsp("foo",3)="loo"
        quit

.. code-block::

    curl -sS localhost:9080/test/json | jq

``curl`` will then return the following output:

.. code-block::

    {
        "foo": [ "boo", "doo", "loo" ]
    }

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How to use Auto Decoding of JSON
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Data received from the browser is automatically decoded and converted to an M array in ``httpreq("json")``. For example, consider the ``posttest`` label mapped in ``_ydbweburl.m``:

.. code-block::

    ;;POST test/post posttest^%ydbwebapi

The label in the ``myjson.m`` M routine:

.. code-block::

    posttest ; POST /test/post Simple test for post
        set httprsp("mime")="text/plain; charset=utf-8" ; Character set of the return URL
        set httprsp="/path/"_httpreq("json","random")_"/1" ; Stored URL
        set httploc=httprsp ; Set the path to your data and return
        quit

This label is accessible via the URL ``/test/post``, e.g.:

.. code-block::

    curl -X POST -H "Content-Type: application/json" localhost:9080/test/post -d '{ "random": "foo" }'

``curl`` will then return the output ``/path/foo/1``.

~~~~~~~~~~
Direct API
~~~~~~~~~~

^^^^^^^^
Encoding
^^^^^^^^

Encoding is done using:

.. code-block::

    do encode^%ydbwebjson(M ARRAY INPUT BY NAME,OUTPUT JSON ARRAY BY NAME,ERROR MESSAGES BY NAME)

The ``encode^%ydbwebjson`` label handles the complexities of JSON encoding, including prefixing 0s to M numeric values less than 0, escaping quotes, etc. Also, the ``encode^%ydbwebjson`` label always succeeds, so passing an error array is unnecessary.

For example:

.. code-block::

        N X,JSON
        S X("myObj","booleanT")="true"
        S X("myObj","booleanF")="false"
        S X("myObj","numeric")=3.1416
        S X("myObj","nullValue")="null"
        S X("myObj","array",1)="one"
        S X("myObj","array",2)="two"
        S X("myObj","array",3)="three"
        S X("myObj","subObject","fieldA")="hello"
        S X("myObj","subObject","fieldB")="world"
        D encode^%ydbwebjson("X","JSON")

        > zwrite JSON
        JSON(1)="{""myObj"":{""array"":[""one"",""two"",""three""],""booleanF"":false,
        ""booleanT"":true,""nullValue"":null,""numeric"":3.1416,
        ""subObject"":{""fieldA"":""hello"",""fieldB"":""world""}}}"

^^^^^^^^
Decoding
^^^^^^^^

Decoding is done using:

.. code-block::

    do decode^%ydbwebjson(JSON ARRAY INPUT BY NAME, M DEST ARRAY BY NAME, ERROR MESSAGES BY NAME)

The first two arguments are required. The third argument is optional; if omitted, error messages will be dumped into ``%ydbwebjsonerr``.

An array input by name consists of a database variable subscripted by array index intengers, e.g.:

.. code-block::

        S ARRAY(1)="ONE"
        S ARRAY(2)="TWO"
        S ARRAY(3)="THREE"

Such an array can be passed to a routine like this:

.. code-block::

    DO decode^%ydbwebjson("MYLOCAL","ARRAY","LOCALERR")

Or:

.. code-block::

    DO decode^%ydbwebjson("MYLOCAL",$NAME(ARRAY),"LOCALERR")

Note that the label uses `name indirection <https://docs.yottadb.com/ProgrammersGuide/langfeat.html#name-indirection>`_ to access the array elements.

For example:

.. code-block::

        YDB>R JSON(1)
        {"title":"my array of stuff", "count":3, "items": [
        YDB>R JSON(2)
        {"name":"red", "rating":"ok"},
        YDB>R JSON(3)
        {"name":"blue", "rating":"good"},
        YDB>R JSON(4)
        {"name":"purple", "rating":"outstanding"}
        YDB>R JSON(5)
        ]}

        YDB>D decode^%ydbwebjson($NA(JSON),$NA(OUT),$NA(ERR))

        YDB>ZWRITE OUT
        OUT("count")=3
        OUT("items",1,"name")="red"
        OUT("items",1,"rating")="ok"
        OUT("items",2,"name")="blue"
        OUT("items",2,"rating")="good"
        OUT("items",3,"name")="purple"
        OUT("items",3,"rating")="outstanding"
        OUT("title")="my array of stuff"

        YDB>ZWRITE ERR
        %YDB-E-UNDEF, Undefined local variable: ERR

Now, let's try an error case by deleting the last brace in the JSON array:

.. code-block::

        YDB>ZWRITE JSON
        JSON(1)="{""title"":""my array of stuff"", ""count"":3, ""items"": ["
        JSON(2)="{""name"":""red"", ""rating"":""ok""},"
        JSON(3)="{""name"":""blue"", ""rating"":""good""},"
        JSON(4)="{""name"":""purple"", ""rating"":""outstanding""}"
        JSON(5)="]}"

        YDB>S JSON(5)="]"

        YDB>K OUT,ERR

        YDB>D decode^%ydbwebjson($NA(JSON),$NA(OUT),$NA(ERR))

        YDB>ZWRITE OUT
        OUT("count")=3
        OUT("items",1,"name")="red"
        OUT("items",1,"rating")="ok"
        OUT("items",2,"name")="blue"
        OUT("items",2,"rating")="good"
        OUT("items",3,"name")="purple"
        OUT("items",3,"rating")="outstanding"
        OUT("title")="my array of stuff"

        YDB>ZWRITE ERR
        ERR(0)=1
        ERR(1)="Stack mismatch - exit stack level was  1"

+++++++++++++++
Other Utilities
+++++++++++++++

~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check for unwanted arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``$$unkargs^%ydbwebutils`` label can be used to check whether any input arguments are missing. If so, the caller must ``QUIT``. In that case, the HTTP error code is set to 111 automatically.

For example:

.. code-block::

        I $$unkargs^%ydbwebutils(.httpargs,"file,iens,field,screen,match") Q  ; Is any of these not passed?

In this example , the code checks that all input variables to a Fileman call are present. If not, it returns an error code of 111. There's no need to set the error manually. Note also that the arguments are passed by reference and the list of fields is passed as a literal.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Converting long lines to an arrays
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A single long line of text can be converted to an array using ``$C(13,10)`` (CR/LF). For instance, this method can be used to convert the body of a ``POST`` or ``PUT`` request into a linear array.

For example, passing the input and output by reference:

.. code-block::

         D parse10^%ydbwebutils(.BODY,.PARSED) ; Parser

Given this value of ``BODY``:

.. code-block::

    BODY(1)="ABC"_$C(13,10)_"DEF"_$C(13,10)_"HIJ"

``parse10^%ydbwebutils`` yields:

.. code-block::

        PARSED(1)="ABC"
        PARSED(2)="DEF"
        PARSED(3)="HIJ"

To reverse the transformation, again passing input by reference:

.. code-block::

        D addcrlf^%ydbwebutils(.RESULTS) ; crlf the result

This is useful when line breaks between word processing fields must be preserved in a given format, e.g. the Fileman format, wherein a CRLF must be added to each line of the result.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sending an HTTP error back to the client
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In case an error is encountered while executing server-side code, an HTTP error code can be sent and execution terminated. The general format for doing this is:

.. code-block::

        D setError^%ydbwebutils(HTTP code,error description) QUIT

For example:

.. code-block::

        D setError^%ydbwebutils("400","Input parameters not correct") Q
