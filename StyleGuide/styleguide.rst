.. ###############################################################
.. #                                                             #
.. # Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.#
.. # All rights reserved.                                        #
.. #                                                             #
.. #     This document contains the intellectual property        #
.. #     of its copyright holder(s), and is made available       #
.. #     under a license.  If you do not know the terms of       #
.. #     the license, please stop and do not read further.       #
.. #                                                             #
.. ###############################################################

===================
Style Guide
===================

.. contents::
   :depth: 5

------------------
Introduction
------------------

This style guide applies to user documentation for all YottaDB software. Existing documentation predating these standards should be reviewed for compliance on material edits, but not necessarily on minor edits such as fixing typos.

---------------------
Language Usage
---------------------

This section focuses on the writing part of the documentation, in regards to the language that must be used.

++++++++++++++
Language
++++++++++++++

Use American English, not British.
For example, use suffix :code:`-ize/-yze/-ization` not :code:`-ise/-yse/-isation`.

A common error in American English is to use the term *waiting on* instead of *waiting for*. While a waiter at a restaurant waits **on** customers, a command or function waits **for** something.

Use Active Voice instead of Passive Voice. This is because active voice conveys a strong, direct and clear tone.
When the *subject* of a sentence performs the verb's action, we say that the sentence is in *active voice*. Although the reader is rarely explicitly addressed in our documentation, he/she is considered to be the *subject*.

For example:

.. code-block:: none

   > Now boot the three virtual machines. (Active voice)

   > The three virtual machines should be booted now. (Passive voice)

++++++++++++
Acronyms
++++++++++++

For a small document the first usage of an acronym should spell out the entire name followed by the acronym in parentheses. The acronym alone may be used in all other instances.

For a large document have a glossary of acronyms with their entire names. Every occurrence of an acronym in the documentation should link to the appropriate entry in the glossary.

Don't use a full stop after an acronym unless it is the last word in a sentence.

++++++++++++
Numbers
++++++++++++

Spell out the numbers 0-9 in full, and use numerals for numbers 10 or greater.

For example:

.. code-block:: none

   > Shut down the Acculturation Workshop virtual machine cleanly and make three copies of the Acculturation Workshop called Paris.vmdk, Melbourne.vmdk and Santiago.vmdk.

   > Paris is at transaction 95 or less.

-------------------------
Documentation Elements
-------------------------

This section focuses on the reStructuredText(rST) elements that are used in the documentation across YottaDB, their syntax and usage.

+++++++++++
Heading
+++++++++++

Text can be underlined or under and overlined to represent a title/heading.

The length of the underline must be at least as long as the title and the length of the under and overline must be identical.

For the purposes of all YottaDB documentation use the second method for representing titles, i.e. both under and overlining the text.

Following examples depict correct titles:

.. code-block:: none

   *****
   Title
   *****

   ########
   subtitle
   ########

   **********************
   subsubtitle
   **********************

The structure of the document is determined from the succession of the headings. Unlike headings in HTML and Markdown, rST does not assign heading levels to certain characters. It is up to the writer of the document to maintain the same convention throughout the project.

Use the following convention for titles in all YottaDB documentation:

.. code-block:: none

    ======, level 1 title (page title)
    ------, level 2 title
    ++++++, level 3 title
    ~~~~~~, level 4 title
    ^^^^^^, level 5 title

If new information is being added to an existing document, it will be easy to identify the succession of headings.

When starting a document from scratch, keep in mind to provide the reader easy access to all topics and sub-topics. This means creating a thorough navigation tree with the help of titles. Depending on the depth of the navigation tree, use an appropriate value for the :code:`:depth:` option of the :code:`.. contents::` directive.

+++++++++++
Text
+++++++++++

Although there is no special syntax in rST to represent text, there are special characters that can be used to emphasize certain text in the documentation.

~~~~~~~~~~~
Bold
~~~~~~~~~~~

To make any text bold in rST, use the following syntax:

.. code-block:: none

   **<Text>**

E.g., \**bold\** will be rendered as **bold**.

The text will not be rendered as intended if it starts or ends with whitespace.
E.g., :code:`**   bold**` will be rendered as :code:`**   bold**` and not **bold**.

Use bold sparingly to emphasize the text that you want to stand out.

~~~~~~~~~~~
Italic
~~~~~~~~~~~

To make any text italic in rST, use the following syntax:

.. code-block:: none

   *<Text>*

E.g., \*italic\* will be rendered as *italic*.

Similar to bold type, text will not be rendered as intended if it starts or ends with whitespace.
E.g., :code:`*italic  *` will be rendered as :code:`*italic   *` and not *italic*.


Also use italic sparingly.

.. note::
   There is no convention used to decide what text should be emphasized using bold or italic. It is up to the writer of the documentation. But the method used to emphasize text should be consistent throughout the document. If you use bold, stick to using bold throughout that document.

~~~~~~~~~~~~
Code Role
~~~~~~~~~~~~

The code role is used to display text as code.

Following is the syntax:

.. code-block:: none

   :code:`<Text>`

which will be rendered as:

   :code:`<Text>`

As seen above, the text is highlighted and displayed using a fixed-width font.

The code role syntax should be used to highlight commands, variable names, directory names,file names etc throughout the documentation.

E.g.:

.. code-block:: none

   The :code:`tree` program shows the environment sourcing :code:`ydb_env_set` creates.

There also exists a :code:`code-block directive`, explained later in this document, which should be used for larger code blocks.

Be careful to not overuse the code role syntax.

+++++++++++
Link
+++++++++++

A link in the document can refer to a website or to another part of the document itself.

~~~~~~~~~~~~~~~
External Link
~~~~~~~~~~~~~~~

To add a link to a website in the documentation, use the following syntax:

.. code-block:: none

  `<insert web link here>`_

For example, **`<\https://docs.yottadb.com/>`_** will be rendered as `<https://docs.yottadb.com/>`_

The underscore at the end is necessary.



A label can be specified for the link in the following manner:

.. code-block:: none

   `Label <insert web link here>`_

For example, **`YottaDB Documentation <\https://docs.yottadb.com/>`_** will be rendered as `YottaDB Documentation <https://docs.yottadb.com/>`_

The space between the label and the link is necessary.

~~~~~~~~~~~~~~~
Internal Link
~~~~~~~~~~~~~~~

All titles in rST are considered as links. But if a title changes, any links referring to it will also have to be changed. This becomes a tedious task, especially in a large document. Thus, we use cross-referencing.

Following is an example of cross-referencing:

.. code-block:: none

   .. _reference-label:

   ---------------------------
   Section to cross-reference
   ---------------------------

   Text goes here.

   It refers to the section itself, see :ref:`reference-label`.

- Cross-referencing uses labels. These labels must be placed just before a section title that needs to be referenced. In the above example the label is :code:`reference-label`
- Label names must be unique throughout the entire documentation.
- The :code:`:ref:` directive must be used to reference the section with the label.
- Cross-referencing works within the same rST document and across documents within the same directory, but not across directories.

Following is the syntax for cross-referencing across directories:

.. code-block:: none

   # AcculturationGuide/acculturation.rst

   For more information refer to the section on `Starting the Source Server <../AdminOpsGuide/dbrepl.html#start-source-server>`_.

.. code-block:: none

   # AdminOpsGuide/dbrepl.rst

   .. _start-source-server:

   ++++++++++++++++++++++++++++
   Starting the Source Server
   ++++++++++++++++++++++++++++

In this case we use a combination of the external and internal Link format. A reference label is defined for the section (in dbrepl.rst), which is then used in the external link (in acculturation.rst).

+++++++++++
List
+++++++++++

~~~~~~~~~~~~~~~~~
Bulleted List
~~~~~~~~~~~~~~~~~

To add a bulleted list to the documentation, use the following syntax:

.. code-block:: none

   * First bulleted list item.
   * Second bulleted list item
     with indentation.

which is rendered as:

   * First bulleted list item.
   * Second bulleted list item
     with indentation.

~~~~~~~~~~~~~~~~~
Numbered List
~~~~~~~~~~~~~~~~~

Although there are two ways to add a numbered list to the documentation, only use the following syntax to do so:

.. code-block:: none

   #. First numbered list item (type 2)
   #. Second numbered list item

which is rendered as:

   #. First numbered list item (type 2)
   #. Second numbered list item

+++++++++++
Note
+++++++++++

Use :code:`note` in rST when you need to alert the user of some important information, including but not limited to warnings, software behavior under certain circumstances etc.

To add a note to the documentation, use the following syntax :

.. code-block:: none

   .. note::
      <Text>

++++++++++++
Code Block
++++++++++++

To add a code-block to the documentation, use the following syntax :

.. code-block:: none

   .. code-block: <language>

      <code snippet>

A new line is essential between :code:`code-block` and the :code:`<code snippet>`, otherwise the code-block accepts the code snippet as a parameter.

Replace :code:`<language>` with the appropriate programming language name. A list of languages supported by the Python syntax highlighter, Pygments, can be found `here <https://pygments.org/languages/>`_. If a particular language is not supported replace :code:`<language>` with :code:`none`.

.. note::
   Indentation is important is rST. Be sure to use proper indentation when using :code:`Note`, :code:`Code Block`, or :code:`List`.

+++++++++++
Table
+++++++++++

Syntax for a simple table is as follows:

.. code-block:: none

   +--------------+---------------+--------------+
   |First column  |Second column  |Third column  |
   +--------------+---------------+--------------+

which will be rendered as:

   +--------------+---------------+--------------+
   |First column  |Second column  |Third column  |
   +--------------+---------------+--------------+


Syntax for a multicell table is as follows:

.. code-block:: none

   +------------+------------+-----------+
   | Header 1   | Header 2   | Header 3  |
   +============+============+===========+
   | body row 1 | column 2   | column 3  |
   +------------+------------+-----------+
   | body row 2 | Cells may span columns.|
   +------------+------------+-----------+
   | body row 3 | Cells may  | - Cells   |
   +------------+ span rows. | - contain |
   | body row 4 |            | - blocks. |
   +------------+------------+-----------+

which will be rendered as:

    +------------+------------+-----------+
    | Header 1   | Header 2   | Header 3  |
    +============+============+===========+
    | body row 1 | column 2   | column 3  |
    +------------+------------+-----------+
    | body row 2 | Cells may span columns.|
    +------------+------------+-----------+
    | body row 3 | Cells may  | - Cells   |
    +------------+ span rows. | - contain |
    | body row 4 |            | - blocks. |
    +------------+------------+-----------+

+++++++++++
Image
+++++++++++

To insert an image into the documentation, use the following syntax:

.. code-block:: none

   .. image:: picture.jpg
