#!/bin/bash
# Run all the tests and exit with value > 0 if any fail.
# Invoke with any argument to get verbose output

if [[ "$PWD" != */test ]]; then
	echo "$(basename $0) must be run in the test directory" >&2
	exit 1
fi

B=../bin

mark_fail () {
	[ -z "$err" ] &&
		{ echo "Error: \$err must be set before running mark_fail" >&2; exit 1; }
	[ $err -gt 0 ] && fail=1
}

pass_fail () {
	[ $err -eq 0 ] && echo "pass" || echo "FAIL"
}

run_test () {
	# Run a program which outputs the string 'failed' if the test fails
	# Because we're testing Fortran programs, there is no portable way
	# of setting an exit status at the moment.
	# Supply one argument, the name of the program: it is assumed to be
	# in the $B directory
	echo -n "$1: "
	"$B/$1" 2>&1 | grep -iqv "failed"; err=$?
	pass_fail
	mark_fail
}

for test in test_{load,save}; do
	run_test $test
done

# Set error state
[ -z "$fail" ] && exit 0 || exit 1