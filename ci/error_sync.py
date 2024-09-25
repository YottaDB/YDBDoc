#!/usr/bin/env python3
#################################################################
#                                                               #
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

# This script checks the following (from bullet 1 and 5 of YDBDoc#409 issue description)
#
# - Check that each message `YDB/sr_port/*.msg` is documented in `YDBDoc/MessageRecovery/errors.rst`.
# - Check that the 1-line message text in `YDBDoc/MessageRecovery/errormsgref.rst` for each message is in
#   sync with the message text in `errors.rst`.
# - Check that the 1-line message text in `YDBDoc/MessageRecovery/errors.rst` for each message is in sync
#   with the message text in `YDB/sr_port/*.msg`.

# If this script generates an invalid RST table in `errormsgref.rst`, sphinx will give a very unhelpful error
# that starts with "Malformed table" and then prints all 3000 lines of the table. You can debug it as follows:
#
# 1. Find where `docutils` is installed for your python version. For me it was in `~/.local/lib/python3.10/site-packages/docutils`.
#    - Usually you can find this directory with `python -m site --user-site`.
# 2. Find where in the source it emits the error: `grep --line-number -r 'Malformed table' ~/.local/lib/python3.10/site-packages/`
# 3. Run sphinx under a python debugger: `python -m pdb $(which sphinx-build) -M html "MessageRecovery" "MessageRecovery/_build" -W -q`.
#    - Note that GDB will not work. It will show frames in the python interpreter, which are not useful (they are all things like `PyEval_Expression`).
# 4. Break at the line you found in step 2: `break ~/.local/lib/python3.10/site-packages/docutils/parsers/rst/states.py:1787`
# 5. Run until that line; go up frames until you find the table block that caused the error.


import argparse
import itertools
import re
import shutil
import sys
import tempfile
import textwrap
from glob import glob


# Constants

ERRORS_RST = "MessageRecovery/errors.rst"
ERROR_MSG_RST = "MessageRecovery/errormsgref.rst"

# Used for comparing the RST file to the msg file.
PLACEHOLDER = "##PLACEHOLDER##"

# Unchecked errors that are from languages wrappers, not YDB itself.
# TODO: parse these and check them in this script.
NOT_YDB_ERRORS = [
    # Go errors: `rg '\sYDB_ERR_[A-Z]+' YDBGo/error_wrapper.go -o | cut -d_ -f 3`
    "STRUCTUNALLOCD",
    "INVLKNMPAIRLIST",
    "DBRNDWNBYPASS",
    "SIGACKTIMEOUT",
    "SIGGORTNTIMEOUT",

    # %GSEL errors
    "ILLEGALUSE",
    "INVALIDGBL",
]

# TODO: document these
UNDOCUMENTED_EXCEPTIONS = [
    "ACK",
    "ENQ",
    "YDIRTSZ",
    "WILLEXPIRE",
    "JNLWRTNOWWRTR",
    "TPRETRY",
    "ZDEFACTIVE",
    "ZDEFOFLOW",
    "JNLREQUIRED",
    "KRNLKILL",
    "FREEZEID",
    "INVDBGLVL",
    "MUDESTROYSUC",
    "MUDESTROYFAIL",
    "REPEATERROR",
    "JOBINTRRQST",
    "JOBINTRRETHROW",
    "BADTAG",
    "ZLINKBYPASS",
    "REPLONLNRLBK",
    "FAKENOSPCLEARED",
    "DBGLDMISMATCH",
    "DRVLONGJMP",
    "LVMONBADVAL",
    "BADPORT",
    "NOTND",
    "OVERRUN",
    "NOSERVENT",
    "BADIPADDRPORT",
]

# These have error messages that look like (xxxx) placeholders.
# Mark the () and [] delimiters as being part of the error message.
NO_AUTO_DELIMITERS = [
    "DBSHMNAMEDIFF",
    "PROCTERM",
    "XTRNRETVAL",
    "GTMSECSHRSRVF",
    "TRIGDEFBAD",
]

# In some cases, we have more information available in the docs than in the .msg
# file. Avoid regressing the docs by showing how the placeholder will appear in
# practice, rather than blindly replacing it with "xxxx".
# {err: {placeholder_str: number of placeholders to replace}}
DOC_EXCEPTIONS = {
    "BKUPPROGRESS": {"tt minutes": 2, "cccc / tttt (pppp%)": 1},
    "TRIGDEFNOSYNC": {"[originating/replicating]": 1, "[replicating/originating]": 1},
    "JNLCREATE": {"<database/region>": 1},
    "JNLSTATE": {"<database/region>": 1},
    "REPLSTATE": {"region/database file": 1},
    # TODO: currently we do not check the second line of INVVALUE for this message, only the first.
    # In practice I think this is ok because they are very similar and updating one will remind you to update the other.
    "INVVALUE": {"DEC": 1, "$ZCONVERT(). Range is -9223372036854775808 to 18446744073709551615": 1 },
}

# modified at runtime so we don't have to pass this mapping through a bunch of functions
# {file_name: {range_start: number of lines to skip}}
MULTILINE_MATCHES = {}


# Parsing and string handling functions

YDB_MSG = re.compile(r"^([A-Z]+)\s*<([^>]*)>")
def parse_ydb_msg_file(filename):
    """Parses a file in YDB/sr_port/*.msg.

    Returns a generator expression that yields (error name, (line number, error description)) tuples.
    """
    with open(filename) as fd:
        for i, line in enumerate(fd):
            if match := re.match(YDB_MSG, line):
                name, desc = match.groups()
                yield name, (f"{filename}:{i+1}", desc)

def load_known_errors():
    """Returns a list of all known errors emitted by YDB."""
    undocumented, documented = {}, {}
    for f in glob("../YDB/sr_port/*.msg"):
        if f.endswith("/cmerrors.msg") or f.endswith("/cmierrors.msg"):
            undocumented.update(parse_ydb_msg_file(f))
        else:
            documented.update(parse_ydb_msg_file(f))

    # Some errors are listed as both documented and undocumented. Treat them as documented.
    for err in documented:
        if err in undocumented:
            del undocumented[err]
    return documented, undocumented

def load_documented_errors(rst):
    """Returns a list of all errors documented in MessageRecovery/errors.rst."""

    """Match a pattern that looks like:
      ----------------------
      LNKNOTIDLE
      ----------------------

      LNKNOTIDLE, Attempt to initiate operation before previous operation completed
    """
    YDBDOC_MSG = re.compile(r"""^---+
([A-Z]+)\s*
---+
\n?\1, (.*)$""", re.MULTILINE)

    errors = {}
    with open(rst) as fd:
        all_docs = fd.read()
        for match in YDBDOC_MSG.finditer(all_docs):
            line_no = all_docs.count('\n', 0, match.start(2))
            errors[match.group(1)] = (line_no, match.group(2))
    return errors


def load_documentation_ref(rst):
    """Returns a list of all errors documented in MessageRecovery/errormsgref.rst."""

    # Match a pattern like "| M16, Argumented QUIT not allowed | * NOTEXTRINSIC, Quit does not return to an extrinsic function"
    # In some cases the description can wrap to a second line. In this case we
    # want to match the wrapped text, and have custom logic to join it together
    # so the wrapping is not considered when comparing.
    YDBDOC_REF = re.compile(r"^(\|.*\| (\* )?)([A-Z]+), ([^|]*)\|(\
\|\s+\| ([^*\|]+)\|)*", re.MULTILINE)
    errors = {}
    MULTILINE_MATCHES[rst] = {}
    with open(rst) as fd:
        all_docs = fd.read()
        for match in YDBDOC_REF.finditer(all_docs):
            line_no = all_docs.count('\n', 0, match.start(3))
            desc = match.group(4).strip()
            if match.group(6) is not None:
                MULTILINE_MATCHES[rst][line_no] = match.group(0).count('\n')
                # the python `re` module doesn't let us access more than one repetition of a capture group.
                # hack around it.
                # TODO: the pypy `regex` module does support this i think.
                for extra_line in match.group(0).splitlines()[1:]:
                    desc += ' ' + extra_line.split('|')[2].strip()
            errors[match.group(3)] = (line_no, desc)
    return errors

def format_ref_line(orig, correction, old_placeholders):
    """Given a line from errormsgref.rst and a correction to the error description, pretty-print the description."""

    # Sphinx chokes on lines that are wider than the header. Wrap the description so this doesn't happen.
    YDBDOC_REF = re.compile(r"^(\| ([0-9]+)?.*\|) (\* )?([A-Z]+), ", re.MULTILINE)
    row = YDBDOC_REF.search(orig)
    assert row is not None, f"logic error: should not be correcting a line that is not a data row: {orig}"

    correction = denormalize_docs(correction, old_placeholders)

    # the error code table has some rows with only an error name, not an error code.
    is_errorcode_table = row.group(2) is not None or row.group(1)[1:-1].strip() == ''
    col1_width, col2_width = (11, 166) if is_errorcode_table else (61, 82)
    if row.group(3) is not None:
        correction = row.group(3) + correction

    wrapped = textwrap.wrap(correction, col2_width)

    first = row[1] + ' ' + wrapped[0].ljust(col2_width) + ' |'
    rest = [ f'|{" "*col1_width}| {line.ljust(col2_width)} |' for line in wrapped[1:] ]
    return "\n".join([first] + rest)


def normalize_msg(err, msg):
    """The .msg and .rst files use different placeholders for variables.
    When we compare the `.msg` files to the RST file, we need them to match.
    This function returns a "normalized" version of the error description for .msg files.
    """

    MSG_PLACEHOLDERS = [
        "!AD",
        "!AZ",
        "!SL",
        "!UJ",
        "!UL",
        "!@UQ",
        "!XJ",
        "!XL",
        "!XW",
        "!16@XQ",
        "!2UL",
        "!@ZQ",
    ]

    # see YDB/sr_unix/util_output.c for what these placeholders mean
    msg = re.sub(r"\s?(![/_^])+\s?", " ", msg)
    msg = re.sub(r"!AZ(AUTOREL|ONLINE|OVERRIDE)", r"[NO]\1", msg)
    # NOTE: this has to be done after we normalize whitespace
    msg = msg.strip()

    for placeholder in MSG_PLACEHOLDERS:
        if err not in NO_AUTO_DELIMITERS:
            placeholder = r"[\[(]?(0x)?" + placeholder + r"[\])]?"
        msg = re.sub(placeholder, PLACEHOLDER, msg)
    return msg


def normalize_docs(err, docs):
    """Like `normalize_msg`, but normalizes the .rst message instead."""

    DOC_PLACEHOLDER = re.compile(r"[\[(]?(0x)?([a-zA-Z])\2{2,4}[\])]?")
    DOC_PLACEHOLDER_NO_DELIMS = re.compile(r"([a-zA-Z])\1{2,4}")

    # this is easier than building it into the regex
    DOC_NOT_A_PLACEHOLDER = {
        "DZWRNOPAREN": "xxx",
    }

    # {index: placeholder_str}
    # we can't just store an array because the order of the placeholders is important.
    existing_placeholders = {}

    if err in DOC_EXCEPTIONS:
        for docstr, num_placeholders in DOC_EXCEPTIONS[err].items():
            def replace_exception(i):
                existing_placeholders[i] = docstr
                return " ".join([PLACEHOLDER]*num_placeholders)
            docs = replace_and_inspect(docs, docstr, replace_exception)

    def replace(match):
        if exclude := DOC_NOT_A_PLACEHOLDER.get(err):
            if match.group(0) == exclude:
                return exclude
        existing_placeholders[match.start()] = match.group(0)
        return PLACEHOLDER

    regex = DOC_PLACEHOLDER_NO_DELIMS if err in NO_AUTO_DELIMITERS else DOC_PLACEHOLDER
    docs = regex.sub(replace, docs)

    existing_placeholders = [existing_placeholders[k] for k in sorted(existing_placeholders.keys())]

    return docs, existing_placeholders

def denormalize_docs(line, old_placeholders):
    """Once a line does not match, we need to update it in place.
    This goes from a line with placeholders, generated from the .msg files, to a line suitable for the .rst file.

    NOTE: this function is "sticky". Placeholders in the .rst file are parsed
    and given first priority when updating. This prevents needless churn
    when only small changes like whitespace or additional text need to be made.
    """

    REPLACEMENTS = [
        "xxxx",
        "yyyy",
        "zzzz",
    ] + [chr(c)*4 for c in range(ord('a'), ord('w'))]

    err = line.split(',', 1)[0]
    parts = line.split(PLACEHOLDER)
    # the `lower()` here is not strictly necessary, but avoids confusing situations where xxxx and XXXX both appear in the same error description.
    replacements = old_placeholders + [r for r in REPLACEMENTS if r not in (p.lower() for p in old_placeholders)]
    replacements = replacements[:len(parts) - 1]

    # if we have an exception that took up more than one placeholder,
    # denormalize it back to only one, not two.
    if err in DOC_EXCEPTIONS:
        for i, part in enumerate(parts):
            if part == " " and len(replacements) >= i - 1:
                old_placeholder = replacements[i - 1]
                if old_placeholder in DOC_EXCEPTIONS[err] and DOC_EXCEPTIONS[err][old_placeholder] > 1:
                    replacements.pop()
                    parts.pop(i)

    return ''.join(intersperse(parts, replacements))


# Helper functions

def intersperse(outer, inner):
    """Given two arrays, combine them into a single array with the values arranged in alternating order.

    >>> intersperse(['abc', 'def', 'ghi'], ['1', '2'])
    ['abc', 1, 'def', 2, 'ghi]
    """
    return (x for x in itertools.chain(*itertools.zip_longest(outer, inner)) if x is not None)

def replace_and_inspect(orig, search, replace):
    """Like re.sub, but for fixed strings.

    >>> replace_and_inspect('abacad', 'a', lambda: 'x')
    'xbxcxd'
    """
    result = ""
    start = 0
    while start < len(orig):
        i = orig.find(search, start)
        if i == -1:
            result += orig[start:]
            break
        result += orig[start:i] + replace(i)
        start = i + len(search)
    return result


# Main functions

def maybe_bless(corrections, failures, rst, bless, corrector):
    if not corrections:
        print(f"NOTE: none of the failures were automatically fixable. You will have to fix the errors manually.")
        return

    tmp = tempfile.NamedTemporaryFile(prefix='errors-', suffix='.rst', mode='w+', delete=False)
    skip = 0
    with open(rst) as fd:
        for i, line in enumerate(fd):
            if skip:
                skip -= 1
                continue
            if fix := corrections.get(i):
                skip = MULTILINE_MATCHES.get(rst, {}).get(i)
                print(corrector(line, fix), file=tmp)
            else:
                print(line, end='', file=tmp)
    tmp.flush()

    print(f"fixed {len(corrections)}/{failures} errors and ", end='')
    if bless:
        shutil.move(tmp.name, rst)
        print(f"updated {rst} in place")
        print(f"HELP: use `git diff {rst}` to see the changes")
    else:
        print(f"generated corrected file at {tmp.name}")
        if len(corrections) != failures:
            print(f"NOTE: {failures-len(corrections)} errors could not be fixed automatically and need to be fixed manually.")
        print(f"HELP: use `git diff --no-index {rst} {tmp.name}` to see the changes")
        print(f"HELP: use `{sys.argv[0]} --bless` to automatically replace {rst}")
    print("NOTE: placeholders are approximate. you may manually edit placeholder names and this script will not change them back.")

def check_missing_errors(msgs, documented, undocumented, filename, make_span):
    failcount = 0
    mismatches = set(documented.keys()).symmetric_difference(set(msgs.keys()))
    for err in mismatches:
        if err not in documented:
            if err in UNDOCUMENTED_EXCEPTIONS:
                continue
            failcount += 1
            span = msgs[err][0]
            print(f"ERROR: {err} was documented in {span} but not in {filename}")
        elif err not in NOT_YDB_ERRORS:  # i.e. this error is documented in the RST but not in YDB
            if err in undocumented:
                # TODO: this case should not be necessary. We should move these errors out of `cmierrors.msg` or change the comment that says they are undocumented.
                continue
            assert err not in msgs, err
            failcount += 1
            span = make_span(documented[err][0])
            print(f"ERROR: {err} was documented in {span} but not in ../YDB/sr_port/*.msg")
    return failcount

def check_errors_rst(args, msgs, documented, undocumented):
    print(f"Checking {args.rst} is in sync with YDB/sr_port/*.msg\n")

    failcount = 0
    corrections = {}

    failcount += check_missing_errors(msgs, documented, undocumented, args.rst, lambda line: f"{args.rst}:{line + 1}")

    for err, (msg_span, msg) in msgs.items():
        if err not in documented:
            continue  # handled above

        doc_span, docs = documented[err]
        nmsg = normalize_msg(err, msg)
        ndocs, old_placeholders = normalize_docs(err, docs)
        if nmsg != ndocs:
            print(f"ERROR: documentation mismatch for {err}")
            print(f"\tError from {msg_span.ljust(33, ' ')}: {nmsg}")
            doc_span_pretty = str(doc_span).ljust(6, ' ')
            print(f"\tError from {args.rst}:{doc_span_pretty}: {ndocs}\n")
            corrections[doc_span] = f"{err}, " + nmsg, old_placeholders
            failcount += 1

    if not failcount:
        print(f"Succeeded: all {len(msgs)} messages in {args.rst} matched")
        return False

    maybe_bless(corrections, failcount, args.rst, args.bless, lambda _, fix: denormalize_docs(*fix))
    print(f"Failed: {failcount}/{len(msgs)}")
    return True

def check_err_msg_ref_rst(args, msgs, documented, undocumented):
    print(f"Checking {args.err_msg_ref} is in sync with YDB/sr_port/*.msg\n")

    failcount = 0
    corrections = {}
    err_msg_documented = load_documentation_ref(args.err_msg_ref)

    failcount += check_missing_errors(msgs, err_msg_documented, undocumented, args.rst, lambda line: f"{args.err_msg_ref}:{line + 1}")

    for err in err_msg_documented:
        if err not in msgs:
            continue  # handled above
        msg_span, msg_desc = msgs[err]
        nmsg = normalize_msg(err, msg_desc)

        ref_line, ref_desc = err_msg_documented[err]
        ndocs, old_placeholders = normalize_docs(err, ref_desc)
        if ndocs == nmsg:
            continue

        failcount += 1
        print(f"ERROR: documentation mismatch for {err}")

        print(f"\tError from {msg_span}: {msg_desc}\n")
        ref_span_pretty = str(ref_line + 1).ljust(5, ' ')
        print(f"\tError from {args.err_msg_ref}:{ref_span_pretty}: {ref_desc}\n")
        print(old_placeholders)
        corrections[ref_line] = f"{err}, " + nmsg, old_placeholders

    if not failcount:
        print(f"Succeeded: all {len(msgs)} messages in {args.err_msg_ref} matched")
        return False

    maybe_bless(corrections, failcount, args.err_msg_ref, args.bless, lambda orig, fix: format_ref_line(orig, *fix))
    print(f"Failed: {failcount}/{len(documented)}")
    return True

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--bless", action="store_true")
    parser.add_argument("rst", nargs="?", default=ERRORS_RST, help="RST error documentation file")
    parser.add_argument("err_msg_ref", nargs="?", default=ERROR_MSG_RST, help="RST error reference file")
    args = parser.parse_args()

    msgs, undocumented = load_known_errors()
    undocumented = list(undocumented.keys()) + UNDOCUMENTED_EXCEPTIONS
    documented = load_documented_errors(args.rst)

    failed  = check_errors_rst(args, msgs, documented, undocumented)
    print()
    failed |= check_err_msg_ref_rst(args, msgs, documented, undocumented)

    exit(failed)

if __name__ == "__main__":
    try:
        main()
    except BrokenPipeError:
        pass
