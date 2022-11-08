# stdlib.sh — A standard library for shell programming

# Atelier (https://github.com/melusina-org/cl-atelier)
# This file is part of Atelier.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT
	 
#
# Logging and Interaction
#

# timestamp [SECONDS-SINCE-EPOCH]
#  Print an ISO 8601 timestamp on stdout
#
# If the argument SECONDS-SINCE-EPOCH is given, the time it refers to
# is used instead of the current time.
#
# It requires GNU Awk available as gawk.

timestamp()
{
    if [ $# -eq 0 ] ;then
        date '+%Y-%m-%dT%H:%M:%SZ'
    else
        env TZ=UTC gawk -v secondssinceepoch="$1" '
BEGIN {
 print strftime("%Y-%m-%dT%H:%M:%SZ", secondssinceepoch);
}'
    fi
}

# epochstamp
#  Print a seconds since epoch timestamp on stdout

epochstamp()
{
    date '+%s'
}


# eprintf PRINTF-LIKE-ARGV
#  Same as printf with output on STDERR
#
# A newline is automatically added to the output.

eprintf()
{
    {
        printf "$@"
        printf '\n'
    } 1>&2
}


# failwith [-x STATUS] PRINTF-LIKE-ARGV
#  Fail with the given diagnostic message
#
# The -x flag can be used to convey a custom exit status, instead of
# the value 1.  A newline is automatically added to the output.

failwith()
{
    local OPTIND OPTION OPTARG status

    status=1
    OPTIND=1

    while getopts 'x:' OPTION; do
        case ${OPTION} in
            x)	status="${OPTARG}";;
            *)	1>&2 printf 'failwith: %s: Unsupported option.\n' "${OPTION}";;
        esac
    done

    shift $(expr ${OPTIND} - 1)

    case "$1" in
	[0-9][0-9][0-9]|[0-9][0-9]|[0-9])
	    status="$1"
	    shift
	    ;;
	*)
	    :
    esac

    wlog 'Error' "$@"
    exit "${status}"
}


# confirm PRINTF-LIKE-ARGV
#  Abort program if the given interactive answer is not yes
#
# The authorized values for yes are: Y, y, yes, YES, Yes.

confirm()
{
    local answer

    printf "$@"
    printf ' [yN] '
    read answer
    case "${answer}" in
        Y|y|yes|YES|Yes)
            : 'NOP'
            ;;
        *)
            failwith 'confirm: The console operator refused to proceed.'
            ;;
    esac
}



# wlog LEVEL PRINTF-LIKE-ARGV
#  Same as printf with output on STDERR
#
# The authorized values for the level are 'Emergency', 'Alert',
# 'Critical', 'Error', 'Warning', 'Notice', 'Info' and 'Debug'.
# The variable wlog_level governs the threshold until which messages
# are silenced out.
#
# A newline is automatically added to the output.

: ${wlog_level:='Info'}

wlog__numeric_level()
{
    local level
    case "$1" in
        Emergency)      level=0;;
        Alert)          level=1;;
        Critical)       level=2;;
        Error)          level=3;;
        Warning)        level=4;;
        Notice)         level=5;;
        Info)           level=6;;
        Debug)          level=7;;
    esac
    printf '%s' "${level}"
}

wlog__is_interesting()
{
    [ $(wlog__numeric_level ${wlog_level}) -ge $(wlog__numeric_level $1) ]
}

wlog()
{
    local level
    level="$1"
    shift

    if wlog__is_interesting "${level}"; then
        {
            printf '%s: %s' "${level}" "${wlog_prefix}${wlog_prefix:+: }"
            printf "$@"
            printf '\n'
        } 1>&2
    fi
}


# hline
#  Print an horizontal line on stdout
#
# This can be used to structure log or reporting output.

hline()
{
    printf '================================================================================\n'
}


# logfile FILENAME
#  Redirect stdin and stderr to FILENAME unless bound to a tty

logfile()
{
    if ! [ -t 1 ]; then
        exec 1>>"$1" 2>&1
    fi
}


#
# Temporary Files and Directories
#

# tmpfile_initializer TMPFILE
#  Create a temporary file
#
# The path to that file is saved in TMPFILE. A hook is registered
# to remove that file upon program termination.

tmpfile_initializer()
{
    local _tmpfile _script
    _tmpfile=$(mktemp -t "${package}-XXXXXX")
    _script=$(printf 'rm -f "%s"' "${_tmpfile}")
    trap "${_script}" INT TERM EXIT
    eval $1="${_tmpfile}"
    export $1
}

# tmpdir_initializer
#  Create a temporary directory
#
# The path to that directory is saved in tmpdir. A hook is registered
# to remove that directory upon program termination.

tmpdir_initializer()
{
    tmpdir=$(mktemp -d -t "${package}-XXXXXX")
    wlog 'Debug' 'tmpdir_initializer: %s' "${tmpdir}"
    trap 'tmpdir_reclaim' INT TERM EXIT
    export tmpdir
}

# tmpdir_reclaim
#  Reclaim the created temporary directory

tmpdir_reclaim()
{
    wlog 'Debug' 'tmpdir_reclaim: %s' "${tmpdir}"
    rm -r -f "${tmpdir:?}"
}

# when PREDICATE COMMAND
#  When PREDICATE is true execute COMMAND

when()
{
    local predicate

    predicate="$1"
    shift

    if "${predicate}"; then "$@"; fi
}

#
# Operation on lines
#

fold_lines()
{
    tr '\n' ',' | sed 's/,$//'
}

first_line()
{
    sed -n '1{p;q;}'
}

# End of file `stdlib.sh'
